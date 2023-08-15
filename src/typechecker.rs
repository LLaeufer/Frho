use crate::types::*;
use crate::utils::*;
use crate::environment::*;
use crate::terms::*;
use crate::logicterms::*;
use crate::values::ValueTypes;

use std::collections::HashMap;

#[derive(PartialEq, Debug, Clone)]
pub enum TypeCheckError {
    WrongType(Type, Type),
    IncompatibleTypes(Type, Type),
    LabelNotPresentInType(Type, Label),
    TypeOrVarNotFound(Label),
    IncompatibleOperation(String, Type),
    IncompatibleBinOperation(Type, String, Type),
    TypeOfFunctionCannotBeDetermined(String),
    CallToYetUnknownRecursiveType,
}

pub type TypeCheckResult = Result<(TypeEnvironment, Type), TypeCheckError>;

pub fn typecheck(type_prediction: bool, env: TypeEnvironment, term: &Term) -> TypeCheckResult {
    match term {
        Term::Block(terms) => typecheck_block_rec(type_prediction, env.new_child(), Type::NoneType, terms, 0),
        _ => typecheck_single_term(type_prediction, env.new_child(), term)
    }
}

fn typecheck_block_rec(type_prediction: bool, env: TypeEnvironment, result: Type, terms: &TermBlock, index: usize) -> TypeCheckResult {
    if terms.len() == index { return Ok((env, result)) }
    let (new_env, new_result) = typecheck_single_term(type_prediction, env, &terms[index])?;
    typecheck_block_rec(type_prediction, new_env, new_result, terms, index+1)
}

pub fn typecheck_single_term(type_prediction: bool, env: TypeEnvironment, term: &Term) -> TypeCheckResult {
    match term {
        Term::Constant(value) => Ok((env, value.get_type())),
        Term::Variable(var) => typecheck_variable(type_prediction, env, var),
        Term::LogicGate(lt) => typecheck_logicgate(type_prediction, env, lt),
        Term::Block(_) => typecheck(type_prediction, env, term),
        Term::Let(var, term) => typecheck_let(type_prediction, env, var, term),
        Term::If(decider, consequence, alternative) => typecheck_if(type_prediction, env, decider, consequence, alternative),
        Term::Function(label, typ, parameters, body) => typecheck_function(type_prediction, env, label, typ, parameters, body),
        Term::AnonymousFunction(parameters, body) => typecheck_anonymous_function(type_prediction, env, parameters, body),
        Term::RecursiveAnonymousFunction(label, typ, parameters, body) => typecheck_anonymous_recursive_function(type_prediction, env, label, typ, parameters, body),
        Term::FunctionCall(term, parameters) => typecheck_function_call(type_prediction, env, term, parameters),
        Term::TypeApplication(block, label) => typecheck_type_application(type_prediction, env, block, label),
        Term::RecordConstruction(variant_vec) => typecheck_record_construction(type_prediction, env, variant_vec),
        Term::RecordUpdate(record, target, new_type) => typecheck_record_update(type_prediction, env, record, target, new_type),
        Term::RecordSelection(record, target) => typecheck_record_selection(type_prediction, env, record, target),
        Term::VariantConstruction(variant) => typecheck_variant_construction(type_prediction, env, variant),
        Term::VariantCase(variant, con_label, con_var, con, alt_var, alt) => typecheck_variant_case(type_prediction, env, variant, con_label, con_var, con, alt_var, alt),
        Term::BigLambda(type_var, typ, term) => typecheck_big_lambda(type_prediction, env, type_var, typ, term),
        Term::Promise(types, _block) => Ok((env, types.clone())),
        Term::Print(_) => Ok((env, Type::NoneType)),
    }

}

pub fn typecheck_variant_case(type_prediction: bool, env: TypeEnvironment, variant: &Term, con_label: &Label, con_var: &Var, con: &Term, alt_var: &Var, alt: &Term) -> TypeCheckResult {
    let (env, variant_type) = typecheck(false, env, variant)?; // We force disable type-prediction here, since we need a valid type in the checks in the later part of this function
    // Should the underling variant evaluation need type-prediction, this can still be enabled by an typecheck step which occurs within this typecheck (at least I hope so)
    match &variant_type {
        Type::VariantType(variant_tup) => {
            let variant_map = field_occ_tuple_to_map(variant_tup.clone());
            match variant_map.get(con_label) {
                None => {},
                Some(occ) => match occ {
                    FieldOccurrence::Absent => {},
                    FieldOccurrence::Present(typ) => {
                        // Label exists in Variant
                        let (env, consequence_type) = typecheck_anonymous_function(type_prediction, env, &vec![(con_var.clone(), typ.clone())], con)?;
                        let (env, alternative_type) = typecheck_anonymous_function(type_prediction, env, &vec![(alt_var.clone(), variant_type)], alt)?;

                        if !consequence_type.is_function() {return Err(TypeCheckError::IncompatibleTypes(Type::FunctionType(vec![], Box::new(Type::NoneType)), consequence_type))}
                        if !alternative_type.is_function() {return Err(TypeCheckError::IncompatibleTypes(Type::FunctionType(vec![], Box::new(Type::NoneType)), alternative_type))}

                        let con_out = consequence_type.get_function_output();
                        let alt_out = alternative_type.get_function_output();

                        if type_prediction {
                            if con_out == &Type::YetUnknownRecursiveType {return Ok((env, alt_out.clone()))}
                            if alt_out == &Type::YetUnknownRecursiveType  {return Ok((env, con_out.clone()))}
                        }
                        if con_out.equal(&alt_out) {return Ok((env, alt_out.clone())); } else {return Err(TypeCheckError::IncompatibleTypes(con_out.clone(), alt_out.clone()));}
                        

                    }
                },
            }

            // Label doesn't exist in Variant
            let (env, res_type) = typecheck_anonymous_function(type_prediction, env, &vec![(alt_var.clone(), variant_type)], alt)?;
            if !res_type.is_function() {return Err(TypeCheckError::IncompatibleTypes(Type::FunctionType(vec![], Box::new(Type::NoneType)), res_type))}
            Ok((env, res_type.get_function_output().clone()))
        },
        _ => return Err(TypeCheckError::WrongType(Type::VariantType(vec![]), variant_type)),
    }
}

pub fn typecheck_big_lambda(type_prediction: bool, env: TypeEnvironment, type_var: &Label, typ: &Type, term: &Term) -> TypeCheckResult {
    let (env, term_type) = typecheck(type_prediction, env, term)?;
    if !typ.equal(&term_type) {return Err(TypeCheckError::WrongType(typ.clone(), term_type))}
    Ok((env, Type::TypeContainerType(type_var.to_string(), Box::new(typ.clone()))))
}

pub fn typecheck_type_application(type_prediction: bool, env: TypeEnvironment, block: &Term, label: &Label) -> TypeCheckResult {
    let (env, typ) = typecheck(type_prediction, env, block)?;
    match typ {
        Type::TypeContainerType(tc_lab, tc_type) => if &tc_lab == label {return Ok((env, *tc_type))} else {return Err(TypeCheckError::TypeOrVarNotFound(label.clone()))},
        _ => Ok((env, typ))
    }
}

pub fn typecheck_record_selection(type_prediction: bool, env: TypeEnvironment, record: &Term, target: &Label) -> TypeCheckResult {
    let (env, record_type) = typecheck(type_prediction, env, record)?;
    match record_type.clone() {
        Type::RecordsType(variants) => {
            let var_map: HashMap<String, FieldOccurrence> = HashMap::from_iter(variants.clone());
            let occurrence = var_map.get(target);
            match occurrence {
                Some(occurrence) => match occurrence {
                    FieldOccurrence::Absent => Err(TypeCheckError::LabelNotPresentInType(record_type, target.clone())),
                    FieldOccurrence::Present(types) => Ok((env, types.clone())),
                }
                None => Err(TypeCheckError::LabelNotPresentInType(record_type, target.clone())),
            }
        },
        _ => Err(TypeCheckError::WrongType(record_type, Type::RecordsType(vec![])))
    }
}

pub fn typecheck_record_update(type_prediction: bool, env: TypeEnvironment, record: &Term, target: &Label, new_type: &Term) -> TypeCheckResult {
    let (env, record_type) = typecheck(type_prediction, env, record)?;
    match record_type.clone() {
        Type::RecordsType(variants) => {
            let mut var_map: HashMap<String, FieldOccurrence> = HashMap::from_iter(variants.clone());
            let occurrence = var_map.get(target);
            match occurrence {
                Some(occurrence) => match occurrence {
                    FieldOccurrence::Absent => Err(TypeCheckError::LabelNotPresentInType(record_type, target.clone())),
                    FieldOccurrence::Present(_present_type) => {
                        let (env, new_type_result) = typecheck(type_prediction, env, new_type)?;
                        var_map.insert(target.clone(), FieldOccurrence::Present(new_type_result));
                        let mut record_vec = vec![];
                        for (label, types) in var_map {
                            record_vec.push((label, types));
                        }

                        Ok((env, Type::RecordsType(record_vec)))
                    },
                },
                None => Err(TypeCheckError::LabelNotPresentInType(record_type, target.clone())),
            }
            
        },
        _ => Err(TypeCheckError::WrongType(record_type, Type::RecordsType(vec![])))
    }
}


fn typecheck_block_vector(type_prediction: bool, env: TypeEnvironment, block_vector: &Vec<Term>) ->  Result<(TypeEnvironment, Vec<Type>), TypeCheckError> {
    fn type_b_v_internal(type_prediction: bool, env: TypeEnvironment, b_v: &Vec<Term>, res: Vec<Type>) ->  Result<(TypeEnvironment, Vec<Type>), TypeCheckError> {
        if b_v.len() == res.len() {
            return Ok((env, res));
        }
        let (eval_env, eval_result) = typecheck(type_prediction, env, &b_v[res.len()])?;
        let mut mut_res = res;
        mut_res.push(eval_result);
        type_b_v_internal(type_prediction, eval_env, b_v, mut_res)
    }
    type_b_v_internal(type_prediction, env, block_vector, vec![])

}

fn compare_type_vecs(first: &Vec<Type>, second: &Vec<Type>) -> bool {
    if first.len() != second.len() {return false}
    let tups = first.into_iter().zip(second).collect::<Vec<_>>();
    for (first_type, second_type) in tups {
        if !first_type.equal(second_type) {return false}
    }
    return true

}

pub fn typecheck_function_call(type_prediction: bool, env: TypeEnvironment, term: &Term, parameters: &Vec<Term>) -> TypeCheckResult {
    let (env, function_type) = typecheck(type_prediction, env, term)?;
    let (env, parameter_types) = typecheck_block_vector(type_prediction, env, parameters)?;
    match function_type {
        Type::FunctionType(input, output) => if compare_type_vecs(&input, &parameter_types) {if !type_prediction && *output == Type::YetUnknownRecursiveType {return Err(TypeCheckError::CallToYetUnknownRecursiveType)} else {return Ok((env, *output.clone()))}} else {return Err(TypeCheckError::WrongType(Type::FunctionType(input, output), Type::FunctionType(parameter_types, Box::new(Type::NoneType))))},
        _ => Err(TypeCheckError::WrongType(function_type, Type::FunctionType(vec![], Box::new(Type::NoneType))))
    }

}

fn types_from_parameter(vars: Vec<VariantType>) -> Vec<Type> {
    fn snd(var_type: VariantType) -> Type {
        let (_var, types) = var_type;
        types
    }
    vars.into_iter().map(|x| snd(x)).collect()
}

pub fn typecheck_anonymous_function(type_prediction: bool, env: TypeEnvironment, parameters: &Vec<VariantType>, body: &Term) -> TypeCheckResult {
    let (_, ret_type) = typecheck(type_prediction, env.new_child_with(HashMap::from_iter(parameters.clone()), HashMap::new()), body)?;
    Ok((env, Type::FunctionType(types_from_parameter(parameters.clone()), Box::new(ret_type))))
}


pub fn typecheck_anonymous_recursive_function(type_prediction: bool, env: TypeEnvironment, label: &Var, typ: &Option<Type>, parameters: &Vec<VariantType>, body: &Term) -> TypeCheckResult {
    let mut parameters_expanded = parameters.clone();
    let parameters_expanded = match typ {
        Some(typ) => {parameters_expanded.push((label.clone(), Type::FunctionType(types_from_parameter(parameters.clone()), Box::new(typ.clone())))); parameters_expanded},
        None => {parameters_expanded.push((label.clone(), Type::FunctionType(types_from_parameter(parameters.clone()), Box::new(Type::YetUnknownRecursiveType)))); parameters_expanded},
    };

    let initial_result = typecheck(type_prediction, env.new_child_with(HashMap::from_iter(parameters_expanded.clone()), HashMap::new()), body);

    // Try to typecheck the function
    let final_result = match initial_result {
        Ok((_, Type::YetUnknownRecursiveType)) => Err(TypeCheckError::TypeOfFunctionCannotBeDetermined(label.clone())),
        // If the result is okay simply return the result
        Ok((_, ret_type)) => Ok((env, Type::FunctionType(types_from_parameter(parameters.clone()), Box::new(ret_type)))),
        // If we cant predict the return of the function, due to a recursive call
        Err(TypeCheckError::CallToYetUnknownRecursiveType) => {
            if type_prediction {Err(TypeCheckError::CallToYetUnknownRecursiveType)} else {
                // We try to predict the type again
                let (passed_trough_env, predicted_type) = typecheck_anonymous_recursive_function(true, env, label, typ, parameters, body)?;
                match predicted_type {
                    // And check the function again, with the predicted return type
                    Type::FunctionType(_, predicted_output) => typecheck_anonymous_recursive_function(type_prediction, passed_trough_env, label, &Some(*predicted_output.clone()), parameters, body),
                    _ => Err(TypeCheckError::WrongType(Type::FunctionType(vec![], Box::new(Type::NoneType)), predicted_type)),
                }
            }
        
        },
        // If we get a generic error just return it
        Err(err) => Err(err),
    };
    final_result

}


pub fn typecheck_function(type_prediction: bool, env: TypeEnvironment, label: &Var, typ: &Option<Type>, parameters: &Vec<VariantType>, body: &Term) -> TypeCheckResult {
    let (mut fun_env, fun_type) = typecheck_anonymous_recursive_function(type_prediction, env, label, typ, parameters, body)?;
    fun_env.insert(label.clone(), fun_type);
    Ok((fun_env, Type::NoneType))
}

pub fn typecheck_record_construction(type_prediction: bool, env: TypeEnvironment, variant_vec: &Vec<RawVariant>) -> TypeCheckResult {
    fn trc_rec(type_prediction: bool, env: TypeEnvironment, vv: &Vec<RawVariant>, index: usize, mut lo: Vec<LabelOccurrence>) -> Result<(TypeEnvironment, Vec<LabelOccurrence>), TypeCheckError> {
        if index == vv.len() {
            return Ok((env, lo));
        }
        let (label, term) = &vv[index];
        let (env, types) = typecheck(type_prediction, env, &term)?;
        lo.push((label.clone(), FieldOccurrence::Present(types)));
        trc_rec(type_prediction, env, vv, index+1, lo)

    }
    let (env, occurrence_vec) = trc_rec(type_prediction, env, variant_vec, 0, vec![])?;
    Ok((env, Type::RecordsType(occurrence_vec)))
}

pub fn typecheck_variant_construction(type_prediction: bool, env: TypeEnvironment, variant: &RawVariant) -> TypeCheckResult {
    let (label, term) = variant;
    let (term_env, term_type) = typecheck(type_prediction, env, term)?;
    Ok((term_env, Type::VariantType(vec![(label.to_string(), FieldOccurrence::Present(term_type))])))
}

pub fn typecheck_logicgate(type_prediction: bool, env: TypeEnvironment, lt: &LogicTerm) -> TypeCheckResult {
    let (fst_env, fst_type) = typecheck(type_prediction, env, lt.fst())?;
    let (snd_env, snd_type) = typecheck(type_prediction, fst_env, lt.snd())?;

    let resulting_type = lt.resulting_type(&fst_type,& snd_type);
    if resulting_type == Type::IllegalType {if lt.is_binary_operation() {return Err(TypeCheckError::IncompatibleBinOperation(fst_type, lt.operation_name(), snd_type))} else 
        {return Err(TypeCheckError::IncompatibleOperation(lt.operation_name(), fst_type))}
    }
    return Ok((snd_env, resulting_type))

}

pub fn typecheck_if(type_prediction: bool, env: TypeEnvironment, decider: &Term, consequence: &Term, alternative: &Term) -> TypeCheckResult {
    let (decider_env, decider_type) = typecheck(type_prediction, env, decider)?;

    // Don't type-check the decider when predicting the type, since that could throw unnecessary errors - Whether the decider is boolean will be checked once we have determined a persumed type
    if !type_prediction && !decider_type.equal(&Type::BoolType) {return Err(TypeCheckError::WrongType(Type::BoolType, decider_type)); }

    let (consequence_env, consequence_type) = typecheck(type_prediction, decider_env, consequence)?;
    let (alternative_env, alternative_type) = typecheck(type_prediction, consequence_env, alternative)?;

    // If we have typeprediction active return the type for the branch which can be determined - Whether this assumption is correct will simply be tested in the next step
    if type_prediction {
        if consequence_type == Type::YetUnknownRecursiveType {return Ok((alternative_env, alternative_type))}
        if alternative_type == Type::YetUnknownRecursiveType {return Ok((alternative_env, consequence_type))}
    }
    if consequence_type.equal(&alternative_type) {return Ok((alternative_env, alternative_type)); } else {return Err(TypeCheckError::IncompatibleTypes(consequence_type, alternative_type));}

}

pub fn typecheck_let(type_prediction: bool, env: TypeEnvironment, var: &Var, term: &Term) -> TypeCheckResult {
    let (mut type_env, term_type) = typecheck(type_prediction, env, term)?;
    type_env.insert(var.to_string(), term_type);
    Ok((type_env, Type::NoneType))
}

pub fn typecheck_variable(type_prediction: bool, env: TypeEnvironment, var: &Var) -> TypeCheckResult {
    match env.get(var) {
        Some(typ) => match typ {
            Type::TypeVariable(type_var) => typecheck_typevar(type_prediction, env, &type_var),
            _ => Ok((env, typ))
        },
        None => Err(TypeCheckError::TypeOrVarNotFound(var.clone())),
    }
}

pub fn typecheck_typevar(_type_prediction: bool, env: TypeEnvironment, type_var: &TypeVar) -> TypeCheckResult {
    match env.type_get(type_var) {
        Some(typ) => Ok((env, typ)),
        None => Err(TypeCheckError::TypeOrVarNotFound(type_var.clone())),
    }
}