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
}

pub type TypeCheckResult = Result<(TypeEnvironment, Type), TypeCheckError>;

pub fn typecheck(env: TypeEnvironment, term: &Term) -> TypeCheckResult {
    match term {
        Term::Block(terms) => typecheck_block_rec(env.new_child(), Type::NoneType, terms, 0),
        _ => typecheck_single_term(env.new_child(), term)
    }
}

fn typecheck_block_rec(env: TypeEnvironment, result: Type, terms: &TermBlock, index: usize) -> TypeCheckResult {
    if terms.len() == index { return Ok((env, result)) }
    let (new_env, new_result) = typecheck_single_term(env, &terms[index])?;
    typecheck_block_rec(new_env, new_result, terms, index+1)
}

pub fn typecheck_single_term(env: TypeEnvironment, term: &Term) -> TypeCheckResult {
    match term {
        Term::Constant(value) => Ok((env, value.get_type())),
        Term::Variable(var) => typecheck_variable(env, var),
        Term::LogicGate(lt) => typecheck_logicgate(env, lt),
        Term::Block(_) => typecheck(env, term),
        Term::Let(var, term) => typecheck_let(env, var, term),
        Term::If(decider, consequence, alternative) => typecheck_if(env, decider, consequence, alternative),
        Term::Function(label, typ, parameters, body) => typecheck_function(env, label, typ, parameters, body),
        Term::AnonymousFunction(parameters, body) => typecheck_anonyous_function(env, parameters, body),
        Term::RecursiveAnonymousFunction(label, typ, parameters, body) => typecheck_anonyous_recursive_function(env, label, typ, parameters, body),
        Term::FunctionCall(term, parameters) => typecheck_function_call(env, term, parameters),
        Term::TypeApplication(block, label) => typecheck_type_application(env, block, label),
        Term::RecordConstruction(variant_vec) => typecheck_record_construction(env, variant_vec),
        Term::RecordUpdate(record, target, new_type) => typecheck_record_update(env, record, target, new_type),
        Term::RecordSelection(record, target) => typecheck_record_selection(env, record, target),
        Term::VariantConstruction(variant) => typecheck_variant_construction(env, variant),
        Term::VariantCase(variant, con_label, con_var, con, alt_var, alt) => typecheck_variant_case(env, variant, con_label, con_var, con, alt_var, alt),
        Term::BigLambda(type_var, typ, term) => typecheck_big_lambda(env, type_var, typ, term),
        Term::Promise(types, _block) => Ok((env, types.clone())),
        Term::Print(_) => Ok((env, Type::NoneType)),
    }

}

pub fn typecheck_variant_case(env: TypeEnvironment, _variant: &Term, _con_label: &Label, _con_var: &Var, con: &Term, _alt_var: &Var, alt: &Term) -> TypeCheckResult {
    let (env, con_type) = typecheck(env, con)?;
    let (env, alt_type) = typecheck(env, alt)?;
    if con_type.equal(&alt_type) {return Ok((env, con_type))} else {return Err(TypeCheckError::IncompatibleTypes(con_type, alt_type))}
}

pub fn typecheck_big_lambda(env: TypeEnvironment, type_var: &Label, typ: &Type, term: &Term) -> TypeCheckResult {
    let (env, term_type) = typecheck(env, term)?;
    if !typ.equal(&term_type) {return Err(TypeCheckError::WrongType(typ.clone(), term_type))}
    // env.type_insert(type_var.clone(), typ.clone());
    // Ok((env, Type::TypeVariable(type_var.clone())))
    Ok((env, Type::TypeContainerType(type_var.to_string(), Box::new(typ.clone()))))
}

pub fn typecheck_type_application(env: TypeEnvironment, block: &Term, label: &Label) -> TypeCheckResult {
    /*match env.type_get(label) {
        Some(typ) => Ok((env, typ)),
        None => Err(TypeCheckError::TypeOrVarNotFound(label.clone())),
    }*/
    let (env, typ) = typecheck(env, block)?;
    match typ {
        Type::TypeContainerType(tc_lab, tc_type) => if &tc_lab == label {return Ok((env, *tc_type))} else {return Err(TypeCheckError::TypeOrVarNotFound(label.clone()))},
        _ => Ok((env, typ))
    }
}

pub fn typecheck_record_selection(env: TypeEnvironment, record: &Term, target: &Label) -> TypeCheckResult {
    let (env, record_type) = typecheck(env, record)?;
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

pub fn typecheck_record_update(env: TypeEnvironment, record: &Term, target: &Label, new_type: &Term) -> TypeCheckResult {
    let (env, record_type) = typecheck(env, record)?;
    match record_type.clone() {
        Type::RecordsType(variants) => {
            let mut var_map: HashMap<String, FieldOccurrence> = HashMap::from_iter(variants.clone());
            let occurrence = var_map.get(target);
            match occurrence {
                Some(occurrence) => match occurrence {
                    FieldOccurrence::Absent => Err(TypeCheckError::LabelNotPresentInType(record_type, target.clone())),
                    FieldOccurrence::Present(_present_type) => {
                        let (env, new_type_result) = typecheck(env, new_type)?;
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


fn typecheck_block_vector(env: TypeEnvironment, block_vector: &Vec<Term>) ->  Result<(TypeEnvironment, Vec<Type>), TypeCheckError> {
    fn type_b_v_internal(env: TypeEnvironment, b_v: &Vec<Term>, res: Vec<Type>) ->  Result<(TypeEnvironment, Vec<Type>), TypeCheckError> {
        if b_v.len() == res.len() {
            return Ok((env, res));
        }
        let (eval_env, eval_result) = typecheck(env, &b_v[res.len()])?;
        let mut mut_res = res;
        mut_res.push(eval_result);
        type_b_v_internal(eval_env, b_v, mut_res)
    }
    type_b_v_internal(env, block_vector, vec![])

}

fn compare_type_vecs(first: &Vec<Type>, second: &Vec<Type>) -> bool {
    if first.len() != second.len() {return false}
    let tups = first.into_iter().zip(second).collect::<Vec<_>>();
    for (first_type, second_type) in tups {
        if !first_type.equal(second_type) {return false}
    }
    return true

}

pub fn typecheck_function_call(env: TypeEnvironment, term: &Term, parameters: &Vec<Term>) -> TypeCheckResult {
    let (env, function_type) = typecheck(env, term)?;
    let (env, parameter_types) = typecheck_block_vector(env, parameters)?;
    match function_type {
        Type::FunctionType(input, output) => if compare_type_vecs(&input, &parameter_types) {return Ok((env, output[0].clone()))} else {return Err(TypeCheckError::WrongType(Type::FunctionType(input, output), Type::FunctionType(parameter_types, vec![])))},
        _ => Err(TypeCheckError::WrongType(function_type, Type::FunctionType(vec![], vec![])))
    }

}

fn types_from_parameter(vars: Vec<VariantType>) -> Vec<Type> {
    fn snd(var_type: VariantType) -> Type {
        let (_var, types) = var_type;
        types
    }
    vars.into_iter().map(|x| snd(x)).collect()
}

pub fn typecheck_anonyous_function(env: TypeEnvironment, parameters: &Vec<VariantType>, body: &Term) -> TypeCheckResult {
    let (_, ret_type) = typecheck(env.new_child_with(HashMap::from_iter(parameters.clone()), HashMap::new()), body)?;
    Ok((env, Type::FunctionType(types_from_parameter(parameters.clone()), vec![ret_type])))
}


pub fn typecheck_anonyous_recursive_function(env: TypeEnvironment, label: &Var, typ: &Option<Type>, parameters: &Vec<VariantType>, body: &Term) -> TypeCheckResult {
    let mut parameters_expanded = parameters.clone();
    let parameters_expanded = match typ {
        Some(typ) => {parameters_expanded.push((label.clone(), Type::FunctionType(types_from_parameter(parameters.clone()), vec![typ.clone()]))); parameters_expanded},
        None => parameters_expanded,
    };

    let (_, ret_type) = typecheck(env.new_child_with(HashMap::from_iter(parameters_expanded.clone()), HashMap::new()), body)?;
    Ok((env, Type::FunctionType(types_from_parameter(parameters.clone()), vec![ret_type])))
}


pub fn typecheck_function(env: TypeEnvironment, label: &Var, typ: &Option<Type>, parameters: &Vec<VariantType>, body: &Term) -> TypeCheckResult {
    let (mut fun_env, fun_type) = typecheck_anonyous_recursive_function(env, label, typ, parameters, body)?; // This needs to replaced with the recursive variant eventually
    fun_env.insert(label.clone(), fun_type);
    Ok((fun_env, Type::NoneType))
}

pub fn typecheck_record_construction(env: TypeEnvironment, variant_vec: &Vec<RawVariant>) -> TypeCheckResult {
    fn trc_rec(env: TypeEnvironment, vv: &Vec<RawVariant>, index: usize, mut lo: Vec<LabelOccurrence>) -> Result<(TypeEnvironment, Vec<LabelOccurrence>), TypeCheckError> {
        if index == vv.len() {
            return Ok((env, lo));
        }
        let (label, term) = &vv[index];
        let (env, types) = typecheck(env, &term)?;
        lo.push((label.clone(), FieldOccurrence::Present(types)));
        trc_rec(env, vv, index+1, lo)

    }
    let (env, occurrence_vec) = trc_rec(env, variant_vec, 0, vec![])?;
    Ok((env, Type::RecordsType(occurrence_vec)))
}

pub fn typecheck_variant_construction(env: TypeEnvironment, variant: &RawVariant) -> TypeCheckResult {
    let (label, term) = variant;
    let (term_env, term_type) = typecheck(env, term)?;
    Ok((term_env, Type::VariantType(vec![(label.to_string(), FieldOccurrence::Present(term_type))])))
}

pub fn typecheck_logicgate(env: TypeEnvironment, lt: &LogicTerm) -> TypeCheckResult {
    let (fst_env, fst_type) = typecheck(env, lt.fst())?;
    let (snd_env, snd_type) = typecheck(fst_env, lt.snd())?;
    /*if !fst_type.equal(&snd_type) { return Err(TypeCheckError::IncompatibleTypes(fst_type, snd_type));}

    if !lt.compatible_operation(&fst_type) { return Err(TypeCheckError::IncompatibleOperation(lt.operation_name(), fst_type))}
    if lt.results_in_bool() {return Ok((snd_env, Type::BoolType))} else {return Ok((snd_env, snd_type))}*/

    let resulting_type = lt.resulting_type(&fst_type,& snd_type);
    if resulting_type == Type::IllegalType {if lt.is_binary_operation() {return Err(TypeCheckError::IncompatibleBinOperation(fst_type, lt.operation_name(), snd_type))} else 
        {return Err(TypeCheckError::IncompatibleOperation(lt.operation_name(), fst_type))}
    }
    return Ok((snd_env, resulting_type))

}

pub fn typecheck_if(env: TypeEnvironment, decider: &Term, consequence: &Term, alternative: &Term) -> TypeCheckResult {
    let (decider_env, decider_type) = typecheck(env, decider)?;
    if !decider_type.equal(&Type::BoolType) {return Err(TypeCheckError::WrongType(Type::BoolType, decider_type)); }

    let (consequence_env, consequence_type) = typecheck(decider_env, consequence)?;
    let (alternative_env, alternative_type) = typecheck(consequence_env, alternative)?;
    if consequence_type.equal(&alternative_type) {return Ok((alternative_env, alternative_type)); } else {return Err(TypeCheckError::IncompatibleTypes(consequence_type, alternative_type));}

}

pub fn typecheck_let(env: TypeEnvironment, var: &Var, term: &Term) -> TypeCheckResult {
    let (mut type_env, term_type) = typecheck(env, term)?;
    type_env.insert(var.to_string(), term_type);
    Ok((type_env, Type::NoneType))
}

pub fn typecheck_variable(env: TypeEnvironment, var: &Var) -> TypeCheckResult {
    match env.get(var) {
        Some(typ) => match typ {
            Type::TypeVariable(type_var) => typecheck_typevar(env, &type_var),
            _ => Ok((env, typ))
        },
        None => Err(TypeCheckError::TypeOrVarNotFound(var.clone())),
    }
}

pub fn typecheck_typevar(env: TypeEnvironment, type_var: &TypeVar) -> TypeCheckResult {
    match env.type_get(type_var) {
        Some(typ) => Ok((env, typ)),
        None => Err(TypeCheckError::TypeOrVarNotFound(type_var.clone())),
    }
}