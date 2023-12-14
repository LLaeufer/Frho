use crate::environment::*;
use crate::types::*;
use crate::utils::*;
use crate::logicterms::*;
use crate::terms::*;
use crate::values::*;

use std::collections::HashMap;
use std::sync::Arc;

#[derive(PartialEq, Debug, Clone)]
pub enum EvaluationError {
    NotYetImplemented(Term),
    VariableNotInEnvironment(String),
    VariableNotOfDesiredType(Value, Type),
    VariableCannotBeReassignedInTheSameBlock(String),
    LogicError(InvalidValueTranslation),
    ExtendedLogicError(Box<Term>, InvalidValueTranslation, Box<Term>),
    Blame(BlameLabel),
    UnknownLogicError,
    UnknownError,
    CustomError(Box<Term>, String),
}

pub type Eval = Result<(Environment, EvaluationResult), EvaluationError>;
pub type EvalResult = Result<EvaluationResult, EvaluationError>;
pub type CustomEval<T> = Result<(Environment, T), EvaluationError>;
pub type CustomEvalResult<T> = Result<T, EvaluationError>;

fn clean_functions(raw: Eval) -> Eval {
    let (mut block_env, block_result) = raw?;
    
    if block_result.should_clean() {
        block_env.remove_parent();
        block_env.cleanup();
    }

    Ok((block_env, block_result))
}

pub fn evaluate(env: Environment, terms: &Term) -> Eval {
    let (_, result) = match terms {
        Term::Block(block) => clean_functions(evaluate_block_rec(env.new_child(), unit_value(), block, 0))?,
        _ => clean_functions(evaluate_single_term(env.new_child(), terms))?,
    };
    // We want to forget the block environment here and reuse the last environment
    Ok((env, result))
}

#[allow(unreachable_patterns)]
pub fn evaluate_single_term(env: Environment, term: &Term) -> Eval {
    match term {
        Term::Constant(value) => Ok((env, value.clone())),
        Term::Variable(var) => evaluate_variable(env, var),
        Term::LogicGate(logic_term) => evaluate_logic(env, logic_term),
        Term::Let(var, let_term) => evaluate_let(env, var, let_term),
        Term::If(decider, consequence, alternative) => evaluate_if(env, decider, consequence, alternative),
        Term::Function(label, parameter, parameter_type, output_type, body) => evaluate_function(env, label, parameter, parameter_type, output_type, body),
        Term::FunctionCall(func, parameter) => evaluate_function_call(env, func, parameter),
        Term::AnonymousFunction(parameter, parameter_type, output_type, body) => evaluate_anonymous_function(env, parameter, parameter_type, output_type, body),
        Term::RecursiveAnonymousFunction(label, parameter, parameter_type, output_type, body) => evaluate_recursive_anonymous_function(env, label, parameter, parameter_type, output_type, body),
        Term::RecordUpdate(record, label, new_value) => evaluate_record_update(env, record, label, new_value),
        Term::RecordSelection(record, label) => evaluate_record_selection(env, record, label),
        Term::RecordConstruction(variants) => evaluate_record_construction(env, variants),
        Term::VariantConstruction(variant) => evaluate_variant_construction(env, variant),
        Term::VariantCase(v_block, des_label, con_var, con, alt_var, alt) => evaluate_variant_case(env, v_block, des_label, con_var, con, alt_var, alt),
        Term::TypeApplication(term_block, types) => evaluate_type_application(env, term_block, types),
        Term::BigLambda(label, kind, term) => evaluate_big_lambda(env, label, kind, term),
        Term::Promise(_, block) => evaluate(env, block),
        Term::Print(term) => evaluate_print(env, term),
        Term::Block(_) => evaluate(env, term),


        Term::Blame(label) => Err(EvaluationError::Blame(label.clone())),
        Term::Cast(term, from, blame, to) => evaluate_cast(env, term, from, blame, to),
        Term::Convertion(term, from, conv_label, to) => evaluate_convertion(env, term, from, conv_label, to),

        _ => Err(EvaluationError::NotYetImplemented(term.clone())),
    }
}
#[warn(unreachable_patterns)]



pub fn evaluate_print(env: Environment, term: &Term) -> Eval {
    let (env, result) = evaluate(env, term)?;
    println!("{}", result);
    Ok((env, unit_value()))
}


pub fn evaluate_variable(env: Environment, var: &String) -> Eval {
    let result = match env.get(var) {
        Some(value) => value.clone(),
        None => return Err(EvaluationError::VariableNotInEnvironment(var.clone())),
    };
    Ok((env, result))
}

pub fn evaluate_let(env: Environment, var: &String, result_term: &Term) -> Eval {
    let (mut new_env, result) = evaluate(env, result_term)?;
    if new_env.insert(var.clone(), result) {
        return Ok((new_env, unit_value()));
    } else {Err(EvaluationError::VariableCannotBeReassignedInTheSameBlock(var.clone()))}
}

fn evaluate_block_rec(env: Environment, result: EvaluationResult, terms: &TermBlock, index: usize) -> Eval {
    if terms.len() == index { return Ok((env, result)) }
    let (new_env, new_result) = evaluate_single_term(env, &terms[index])?;
    evaluate_block_rec(new_env, new_result, terms, index+1)
}

pub fn evaluate_function_block(env: &Environment, terms: &Term, parameter: HashMap<Var, Value>) -> EvalResult {
    let (_, result) = match terms {
        Term::Block(block) => evaluate_block_rec(env.new_child_with(parameter), unit_value(), block, 0)?,
        _ => evaluate_single_term(env.new_child_with(parameter), terms)?,
    };
    // We want to forget the block environment here and reuse the last environment
    Ok(result)
}


pub fn evaluate_if(env: Environment, decider: &Term, consequence: &Term, alternative: &Term) -> Eval {
    let (env, decider_result) = evaluate(env, decider)?;
    match decider_result {
        Value::VBool(true) => evaluate(env, consequence),
        Value::VBool(false) => evaluate(env, alternative),
        Value::VDynamic(value, _basetype, blame) => if value.get_type().is_base_type_and(&BaseType::BoolType) {
            evaluate_if(env, &Term::Constant(*value), consequence, alternative)
        } else {
            Err(EvaluationError::Blame(blame))
        }
        ,
        _ => Err(EvaluationError::UnknownLogicError),
    }
}

pub fn evaluate_function(env: Environment, label: &Label, parameter: &Var, parameter_type: &Type, output_type: &Type, body: &Term) -> Eval {
    let mut mut_env = env;
    let func_env = mut_env.new_child();
    if mut_env.insert(label.clone(), Value::VFunc(Box::new(func_env), parameter.clone(), Box::new(Term::Block(Arc::new(vec![body.clone()]))), Type::FunctionType(Box::new(parameter_type.clone()), Box::new(output_type.clone())))) {
        return Ok((mut_env, unit_value())); 
    }
    Err(EvaluationError::VariableCannotBeReassignedInTheSameBlock(label.clone()))
}

pub fn evaluate_anonymous_function(env: Environment, parameter: &Var, parameter_type: &Type, output_type: &Type, body: &Term) -> Eval {
    let func_env = env.new_child();
    Ok((env, Value::VFunc(Box::new(func_env), parameter.clone(), Box::new(Term::Block(Arc::new(vec![body.clone()]))), Type::FunctionType(Box::new(parameter_type.clone()), Box::new(output_type.clone())))))
}

pub fn evaluate_recursive_anonymous_function(env: Environment, label: &Label, parameter: &Var, parameter_type: &Type, output_type: &Type, body: &Term) -> Eval {
    let new_body = Term::Block(Arc::new(vec![Term::Function(label.clone(), parameter.clone(), parameter_type.clone(), output_type.clone(), Box::new(body.clone())), Term::FunctionCall(Box::new(Term::Variable(label.clone())), Box::new(Term::Variable(parameter.clone())))]));
    let func_env = env.new_child();
    Ok((env, Value::VFunc(Box::new(func_env), parameter.clone(), Box::new(Term::Block(Arc::new(vec![new_body.clone()]))), Type::FunctionType(Box::new(parameter_type.clone()), Box::new(output_type.clone())))))
}

pub fn parameter_to_call_arguments(vars: Vec<VariantType>) -> Vec<Term> {
    fn fst(var_type: VariantType) -> Term {
        let (var, _types) = var_type;
        Term::Variable(var)
    }
    let vars: Vec<Term> = vars.into_iter().map(|x| fst(x)).collect();
    vars

}

pub fn map_from_parameter(vars: Vec<VariantType>, values: Vec<Value>) -> HashMap<Var, Value> {
    fn fst(var_type: VariantType) -> Var {
        let (var, _types) = var_type;
        var
    }
    let vars: Vec<Var> = vars.into_iter().map(|x| fst(x)).collect();
    std::iter::zip(vars, values).collect()
}

pub fn evaluate_function_call(env: Environment, func_body_location: &Term, parameter: &Term) -> Eval {
    // Evaluate input for the function call later
    let (env, param_results) = evaluate(env, parameter)?;
    // Search for value matching the function name
    let (env, maybe_func) = evaluate(env, func_body_location)?;
        // Test whether value is a function
    match maybe_func {
        // Execute the function -- Normal case
        Value::VFunc(func_env, params, body, _) => {
            let mut param_map = HashMap::new();
            param_map.insert(params.clone(), param_results);
            Ok((env, evaluate_function_block(&func_env, &body, param_map)?))
        },
        // rho |- M --> VCast u (A1 -> B1) p (A2 -> B2)
	    // rho |- N --> v
	    // rho |- (u (v : A2 =[-p]=> A1)) : B1 =[p]=> B2 --> w
        // ----------------------------------
	    // rho |- M(N) --> w
        Value::VCast(vfunc, fun_type1, blame_label, fun_type2) => {
            let (a1, b1) = fun_type1.get_function_types().expect("evaluate_function_call: Error while handling VCast");
            let (a2, b2) = fun_type2.get_function_types().expect("evaluate_function_call: Error while handling VCast");
            let second_cast = Term::Cast(Box::new(Term::Constant(param_results)), a2.clone(), blame_label.invert(), a1.clone());
            let funcall = Term::FunctionCall(Box::new(Term::Constant(*vfunc.clone())), Box::new(second_cast));
            let cast = Term::Cast(Box::new(funcall), b1.clone(), blame_label.clone(), b2.clone());
            evaluate(env, &cast)
        }
        // Value isn't a function
        _ => Err(EvaluationError::VariableNotOfDesiredType(maybe_func, Type::FunctionType(Box::new(unit()), Box::new(unit()))))
    }
}

pub fn evaluate_record_update(env: Environment, record: &Term, label: &Label, new_value: &Term) -> Eval {
    let (env, record_result) = evaluate(env, record)?;
    let (env, new_value_result) = evaluate(env, new_value)?;
    match record_result {
        Value::VRecord(map) => {
            let mut new_map = map.clone();
            new_map.insert(label.clone(), new_value_result);
            Ok((env, Value::VRecord(new_map)))
        },
        Value::VCast(val, type1, blame, type2) => {
            // rho |- M --> VCast v { r1 } p { r2 }
	        // rho |- v.ell = N --> w
	        // ----------------------------------------------------------------------
	        // rho |- M.ell = N --> VCast w { l:+A; r1\{l} } p { l:+A; r2\{l} }
            let (env, subresult) = evaluate_record_update(env, &Term::Constant(*val), label, new_value)?;
            let cast_type1 = record_type(type1, blame.clone())?;
            let cast_type2 = record_type(type2, blame.clone())?;
            let cast_type1 = match cast_type1 {
                Type::RecordsType(occs, is_dyn) => {
                    let mut occs_map = field_occ_tuple_to_map(occs);
                    occs_map.insert(label.clone(), FieldOccurrence::Present(new_value_result.get_type()));
                    Type::RecordsType(field_occ_map_to_tuple(occs_map), is_dyn).normalize()

                },
                _ => cast_type1,
            };
            let cast_type2 = match cast_type2 {
                Type::RecordsType(occs, is_dyn) => {
                    let mut occs_map = field_occ_tuple_to_map(occs);
                    occs_map.insert(label.clone(), FieldOccurrence::Present(new_value_result.get_type()));
                    Type::RecordsType(field_occ_map_to_tuple(occs_map), is_dyn).normalize()

                },
                _ => cast_type2,
            };
            if cast_type1 == cast_type2 {
                Ok((env, subresult))
            } else {
                Ok((env, Value::VCast(Box::new(subresult), cast_type1, blame, cast_type2)))
            }
        },
        Value::VSealPostConv(subvalue, type1, conv, type2) => {
            let (env, subresult) = evaluate_record_update(env, &Term::Constant(*subvalue), label, new_value)?;
            let (mut conv1_occs, conv1_dyn) = match type1 {
                Type::RecordsType(occs, is_dyn) => {
                    (field_occ_tuple_to_map(occs), is_dyn)
                }
                _ => return Err(EvaluationError::UnknownError)
            };

            let (mut conv2_occs, conv2_dyn) = match type2 {
                Type::RecordsType(occs, is_dyn) => {
                    (field_occ_tuple_to_map(occs), is_dyn)
                }
                _ => return Err(EvaluationError::UnknownError)
            };

            conv1_occs.insert(label.clone(), FieldOccurrence::Present(new_value_result.get_type()));
            conv2_occs.insert(label.clone(), FieldOccurrence::Present(new_value_result.get_type()));
            let new_conv1 = Type::RecordsType(field_occ_map_to_tuple(conv1_occs), conv1_dyn).normalize();
            let new_conv2 = Type::RecordsType(field_occ_map_to_tuple(conv2_occs), conv2_dyn).normalize();
            if new_conv1 == new_conv2 {
                Ok((env, subresult))
            } else {
                Ok((env, Value::VSealPostConv(Box::new(subresult), new_conv1, conv, new_conv2)))
            }
        },
        _ => Err(EvaluationError::VariableNotOfDesiredType(record_result, unit()))
    }
}

fn record_type(record: Type, blame: BlameLabel) -> Result<Type, EvaluationError> {
    match record {
        Type::DynType => Ok(Type::DynType),
        Type::RecordsType(_, _) => { 
            Ok(record)
        }
        _ => Err(EvaluationError::Blame(blame)),
    }
}

fn record_cast_type(record: Type, blame: BlameLabel, label: &String) -> Result<Type, EvaluationError> {
    match record {
        Type::DynType => Ok(Type::DynType),
        Type::RecordsType(occs, end) => {
            let occs_map = field_occ_tuple_to_map(occs);
            match occs_map.get(label) {
                Some(occ) => match occ {
                    FieldOccurrence::Absent =>  Err(EvaluationError::Blame(blame)),
                    FieldOccurrence::Present(subtype) => Ok(subtype.clone()),
                    FieldOccurrence::Star => Ok(Type::DynType),
                },
                None => if end == RecordAndVariantEnd::DynamicEnd { Ok(Type::DynType) } else {
                    return Err(EvaluationError::Blame(blame))
                },
            }
        }
        _ => Err(EvaluationError::Blame(blame)),
    }
}

pub fn evaluate_record_selection(env: Environment, record: &Term, label: &Label) -> Eval {
    let (env, record_result) = evaluate(env, record)?;
    match record_result {
        Value::VRecord(map) => Ok((env, map[label].clone())),
                    
                    // rho |- M --> VCast (VCast v { l:-; r1 } p { l:(*) }) 
                    //                    { l:(*); r2 } q { l:+B; r3 }
                    // -------------------------------------------------------
                    // rho |- M.ell --> blame q

                    // rho |- M --> VCast (VCast v { l:+A; r1 } p { l:(*) }) 
	                //                    { l:(*); r2 } q { l:+B; r3 }
                    // rho |- ((v.ell) : A =[p]=> *) * =[q]=> B --> w
	                // -------------------------------------------------------
	                // rho |- M.ell --> w

                    // rho |- M --> VCast (VCast v { l:(*); r1 } p { l:(*) }) 
                    //              { l:(*); r2 } q { l:+B; r3 }
                    // rho |- (VCast v { l:(*); r1 } q  { l:+B; * }).ell --> w
                    // -------------------------------------------------------
                    // rho |- M.ell --> w

                    // rho |- M --> VCast v { l:+A; r1 } p { l:+B; r2 }
                    // rho |- (v.ell) : A =[p]=> B --> w
                    // -------------------------------------------------------
                    // rho |- M.ell --> w
        Value::VCast(internal, type1, blame, type2) => {
            match *internal.clone() {
                Value::VCast(_, t1, _, _) => {
                    match t1 {
                        Type::RecordsType(occs, _) => {
                            let occs_map = field_occ_tuple_to_map(occs);
                            match occs_map.get(label) {
                                Some(occ) => match occ {
                                    FieldOccurrence::Absent => return Err(EvaluationError::Blame(blame)),
                                    _ => {}
                                },
                                _ => {},
                            }

                        },
                        _ => {}
                    }
                },
                _ => {},

            }
            let cast_type1 = record_cast_type(type1, blame.clone(), label)?;
            let cast_type2 = record_cast_type(type2, blame.clone(), label)?;
            let sub_term = Term::RecordSelection(Box::new(Term::Constant(*internal)), label.clone());
            let cast = Term::Cast(Box::new(sub_term), cast_type1, blame, cast_type2);
            evaluate(env, &cast)

        },
        Value::VSealPostConv(subvalue, type1, conv, type2) => {
            let conv1_occ = match type1 {
                Type::RecordsType(occs, _) => {
                    let mymap = field_occ_tuple_to_map(occs);
                    let mby_occ = mymap.get(label);
                    match mby_occ {
                        Some(occ) => occ.clone(),
                        None => return Err(EvaluationError::UnknownError),
                    }
                }
                _ => return Err(EvaluationError::UnknownError)
            };
            let conv2_occ = match type2 {
                Type::RecordsType(occs, _) => {
                    let mymap = field_occ_tuple_to_map(occs);
                    let mby_occ = mymap.get(label);
                    match mby_occ {
                        Some(occ) => occ.clone(),
                        None => return Err(EvaluationError::UnknownError),
                    }
                }
                _ => return Err(EvaluationError::UnknownError)
            };
            let sub_term = Term::RecordSelection(Box::new(Term::Constant(*subvalue)), label.clone());
            if conv1_occ.is_present() && conv2_occ.is_present() {
                let conv1_type = conv1_occ.to_type().expect("Error in evaluate_record_selection");
                let conv2_type = conv2_occ.to_type().expect("Error in evaluate_record_selection");
                evaluate_convertion(env, &sub_term, &conv1_type, &conv, &conv2_type)
            } else {
                evaluate(env, &sub_term)
            }

        },
        
        _ => Err(EvaluationError::VariableNotOfDesiredType(record_result, unit()))
    }
}

pub type Variant = (Label, Value);

pub fn evaluate_record_construction(env: Environment, unevaluated_record: &Vec<RawVariant>) -> Eval {
    fn eval_rec_con_rec(env: Environment, un_rec: &Vec<RawVariant>, mut rec: Vec<Variant>) -> CustomEval<Vec<Variant>> {
        if un_rec.len() == rec.len() { return Ok((env, rec)); }
        let (label, terms) = &un_rec[rec.len()];
        let (env, result) = evaluate(env, &terms)?;
        rec.push((label.clone(), result));
        eval_rec_con_rec(env, un_rec, rec)
    }

    let (env, variants) = eval_rec_con_rec(env, &unevaluated_record, vec![])?;
    let map = value_tuple_to_map(variants);
    Ok((env, Value::VRecord(map)))

}

pub fn evaluate_variant_construction(env: Environment, unevaluated_variant: &RawVariant) -> Eval {
    let (variant_label, variant_term) = unevaluated_variant; 
    let (env, variant_result) = evaluate(env, &variant_term)?;
    Ok((env, Value::VVariant(variant_label.clone(), Box::new(variant_result))))
}

pub fn evaluate_variant_case(env: Environment, variant_block: &Term, desired_label: &Label, consequence_var: &Var, consequence: &Term, alternative_var: &Var, alternative: &Term) -> Eval {
    let (env, maybe_variant) = evaluate(env, variant_block)?;
    let (env, label, value) = get_variant_label_and_value(env, maybe_variant)?;
    if &label == desired_label {
        let result = evaluate_function_block(&env, &consequence, value_tuple_to_map(vec![(consequence_var.clone(), value)]))?;
        Ok((env, result))
    } else {
        let result = evaluate_function_block(&env, &alternative, value_tuple_to_map(vec![(alternative_var.clone(), Value::VVariant(label, Box::new(value)))]))?;
        Ok((env, result))
    }
}

pub fn get_variant_label_and_value(env: Environment, value: Value) -> Result<(Environment, String, Value), EvaluationError> {
    match value {
        Value::VVariant(label, value) => Ok((env, label, *value)),
        Value::VSealPostConv(subvalue, type1, conv, type2) => {
            let (env, label, value) = get_variant_label_and_value(env, *subvalue)?;
            let conv1_occ = match type1 {
                Type::VariantType(occs, _) => {
                    let mymap = field_occ_tuple_to_map(occs);
                    let mby_occ = mymap.get(&label);
                    match mby_occ {
                        Some(occ) => occ.clone(),
                        None => return Err(EvaluationError::UnknownError),
                    }
                }
                _ => return Err(EvaluationError::UnknownError)
            };
            let conv2_occ = match type2 {
                Type::VariantType(occs, _) => {
                    let mymap = field_occ_tuple_to_map(occs);
                    let mby_occ = mymap.get(&label);
                    match mby_occ {
                        Some(occ) => occ.clone(),
                        None => return Err(EvaluationError::UnknownError),
                    }
                }
                _ => return Err(EvaluationError::UnknownError)
            };
            if conv1_occ.is_present() && conv2_occ.is_present() {
                let conv1_type = conv1_occ.to_type().expect("Error in evaluate_record_selection");
                let conv2_type = conv2_occ.to_type().expect("Error in evaluate_record_selection");
                let (env, value) = evaluate_convertion(env, &Term::Constant(value), &conv1_type, &conv, &conv2_type)?;
                Ok((env, label, value))
            } else {
                Ok((env, label, value))
            }


        },
        Value::VCast(internal, _type1, _blame, _type2) => {            
            let (label, value) = unwrap_vcast_variant(&*internal);
            Ok((env, label, value))
        },
        _ => Err(EvaluationError::VariableNotOfDesiredType(value, Type::VariantType(vec![], RecordAndVariantEnd::Closed)))
    }
}

pub fn evaluate_type_application(env: Environment, block: &Term, types: &Type) -> Eval {
    // rho, sigma |- M --> VAll X K v A, sigma'
	// rho, sigma'[alpha = B] |-conv v[alpha/X] : A[alpha/X] ~[+alpha]~> A[B/X] --> w
    // --------------------------------------------------------------------- alpha fresh
    // rho, sigma |- M <B>  -->  w, sigma'[alpha = B]

    let (env, block_result) = evaluate(env, block)?;
    match block_result {
        Value::VAll(x, _kind, internal) => {
            match *internal.clone() {
                Value::VSeal(value , subtype, _typename) => if subtype == Type::TypeVariable(x.clone()) {return Ok((env, *value))},
                Value::VSealPostConv(value, type1, conv, type2) => if type1.is_record() && type2.is_record() && ! conv.is_present(){
                    let (occs1, dyn1) = type1.get_record_fields().expect("Error in evaluate_type_application");
                    let (mut occs2, dyn2) = type2.get_record_fields().expect("Error in evaluate_type_application");
                    for i in 0..occs2.len() {
                        let (_ , occ) = occs2[i].clone();
                        match occ {
                            FieldOccurrence::Present(subtype) => if subtype == Type::TypeVariable(x.clone()) {occs2[i] = occs1[i].clone()},
                            _ => {},
                        }
                    }
        
                    return Ok((env, Value::VSealPostConv(value, Type::RecordsType(occs1, dyn1), conv.clone(), Type::RecordsType(occs2, dyn2))))
                    
                }
                _ => {},
            }


            let new_val = internal.try_replace_type(&internal.get_type().replace_target(&Type::TypeVariable(x.clone()), types));
            match new_val {
                Some(new_val) => Ok((env, new_val)),
                None => Ok((env, *internal)),
            }
        },
        Value::VCast(subvalue, input_type, blame, output_type) => {
            let (env, subvalue_result) = evaluate_type_application(env, &Term::Constant(*subvalue), types)?;
            let new_in = match input_type {
                Type::UniversalType(x, _, subtype) => subtype.replace_target(&Type::TypeVariable(x.clone()), types),
                _ => input_type
            };
            let new_out = match output_type {
                Type::UniversalType(x, _, subtype) => subtype.replace_target(&Type::TypeVariable(x.clone()), types),
                _ => output_type
            };
            Ok((env, Value::VCast(Box::new(subvalue_result), new_in, blame, new_out)))

        },
        // This needs probably to be fixed
        _ => Ok((env, block_result))
    }
}

pub fn evaluate_big_lambda(env: Environment, label: &Label, kind: &Kind, term: &Term) -> Eval {
    let (env, value) = evaluate(env, term)?;
    Ok((env, Value::VAll(label.clone(), kind.clone(), Box::new(value))))
}

pub fn evaluate_cast(env: Environment, term: &Term, type1: &Type, blame_label: &BlameLabel, type2: &Type) -> Eval {
    let type1 = &type1.normalize();
    let type2 = &type2.normalize();

    let (new_env, result) = evaluate(env, term)?;
    
    // rho |-cast v : * =[p]=> *  --> v
    if type1 == type2 && type1.is_dyn() {return Ok((new_env, result))}
    
    // rho |-cast v : G =[p]=> *  -->   VDynamic v G p
    if type1.is_ground_type(false) && type2.is_dyn() {return Ok((new_env, Value::VDynamic(Box::new(result), 
        type1.to_ground_type(false).expect("evaluate_cast: Error in: rho |-cast v : G =[p]=> *  -->   VDynamic v G p"), blame_label.clone())))}

    //  A is neither dynamic nor a ground type, but A ~ G
    //  rho |-cast v : A =[p]=> * --> VDynamic (VCast v A p G) G p
    if !type1.is_dyn() && !type1.is_ground_type(false) && type1.is_ground_type(true) && type2.is_dyn() {
        let ground_type = type1.to_ground_type(true).expect("evaluate_cast: Error in rho |-cast v : A =[p]=> * --> VDynamic (VCast v A p G) G p");
        return Ok((new_env, Value::VDynamic(Box::new(Value::VCast(Box::new(result), type1.clone(), blame_label.clone(), ground_type.to_type())), ground_type, blame_label.clone())))
    }

    //  A is neither dynamic nor a ground type, but A ~ G
    //  rho |-cast v : * =[p]=> G --> w
	//  ------------------------------------------------------------
    //  rho |-cast v : * =[p]=> A --> VCast w G p A
    
    if type1.is_dyn() && !type2.is_dyn() && !type2.is_ground_type(false) && type2.is_ground_type(true) {
        let ground_type = type2.to_ground_type(true).expect("evaluate_cast: Error in: rho |-cast v : * =[p]=> A --> VCast w G p A");

        let (new_env, new_result) = evaluate(new_env, &Term::Cast(Box::new(Term::Constant(result.clone())), Type::DynType, blame_label.clone(), ground_type.to_type()))?;
        return Ok((new_env, Value::VCast(Box::new(new_result), ground_type.to_type(), blame_label.clone(), type2.clone())))
    }

    // rho |-cast (VDynamic v G p) : * =[q]=> G --> v
    // and
    // G != H
    // ------------------------------------------------------------
    // rho |-cast (VDynamic v G p) : * =[q]=> H --> Blame q
    match result.clone() {
        Value::VDynamic(value, ground_type, _blame_label) => if type1.is_dyn() && type2.is_ground_type(false) {
            if ground_type == type2.to_ground_type(false).expect("evaluate_cast: Error in: rho |-cast (VDynamic v G p) : * =[q]=> G --> v") {return Ok((new_env, *value))} else {return Err(EvaluationError::Blame(blame_label.clone()))}
        },
        _ => {}
    }

    // A is a base type
    // rho |-cast v : A p A --> v
    // and
    // rho |-cast v : alpha p alpha --> v

    if type1 == type2 && (type1.is_base_type() || type2.is_type_name()) {return Ok((new_env, result))}

    // rho |- (app v <*>) : A[*/x] =[p]=> B --> w
    // --------------------------------------------- if QPoly(B)
    // rho |-cast v : (all X:K. A) =[p]=> B --> w
    if type1.is_universal() && type2.is_qpoly() {
        let (tvar, _kind, internal_type) = match type1.clone() {
            Type::UniversalType(tvar, kind, former_type) => (tvar, kind, former_type),
            _ => panic!("evaluate_cast: Error in: rho |-cast v : (all X:K. A) =[p]=> B --> w"),
        };
        let (new_env, univeral_result) = evaluate_type_application(new_env, &Term::Constant(result), &Type::DynType)?;
        return evaluate_cast(new_env, &Term::Constant(univeral_result), &internal_type.replace_target(&Type::TypeVariable(tvar), &Type::DynType), blame_label, type2)


    }


    // ---------------------------------------------------------------------- if QPoly (A)
    // rho |-cast v : A =[p]=> all X:K.B  -->  lam <X:K> (v : A =[p]=> B)
    if type1.is_qpoly() && type2.is_universal() {
        match type2.clone() {
            Type::UniversalType(tvar, kind, former_type) => {
                let cast = Term::Cast(Box::new(Term::Constant(result.clone())), type1.clone(), blame_label.clone(), *former_type.clone());
                let big_lam = Term::BigLambda(tvar, kind, Box::new(cast));
                // let big_lam = Term::BigLambda(tvar, kind, Box::new(cast), *former_type.clone());
                return evaluate(new_env, &big_lam)
            },
            _ => panic!("evaluate_cast: Error in: rho |-cast v : A =[p]=> all X:K.B  -->  lam <X:K> (v : A =[p]=> B)")
        }
    }

    //      rho |-cast (VVariant l v) <l:+A1, r1> =>p <l:+A2, r2>   --->
    //      VVariant l (v : A1 =>p A2)

    //      rho |-cast (VVariant l v) <l:+A1, r1> =>p <l:-, r2>   --->
    //      blame p
    if type1.is_variant() && type2.is_variant() && result.get_type().is_variant() {
        let (occs1, _dyn1) = type1.get_variant_fields().expect("Error in evaluate_cast");
        let (occs2, _dyn2) = type2.get_variant_fields().expect("Error in evaluate_cast");
        /*let (var_lab, value) = match result {
            Value::VVariant(label, value) => (label, value),
            _ => unwrap_vcast_variant(&result),
            // _ => panic!("Error in evaluate_cast: {}", result)

        };*/
        let (new_env, var_lab, value) = get_variant_label_and_value(new_env, result)?;

        let occs1_map = field_occ_tuple_to_map(occs1);
        let occs2_map = field_occ_tuple_to_map(occs2);
        match occs2_map.get(&var_lab) {
            Some(field_occ) => match field_occ {
                FieldOccurrence::Present(type2_subtype) => {
                    match occs1_map.get(&var_lab) {
                        Some(type1_field_occ) => match type1_field_occ {
                            FieldOccurrence::Present(type1_subtype) => {
                                let (new_env, new_value) = evaluate_cast(new_env, &Term::Constant(value), type1_subtype, blame_label, type2_subtype)?;
                                return Ok((new_env, Value::VVariant(var_lab, Box::new(new_value))))
                            },
                            _ => return Err(EvaluationError::Blame(blame_label.clone())),

                        },
                        None => return Err(EvaluationError::Blame(blame_label.clone())),
                    }
                },
                _ => return Err(EvaluationError::Blame(blame_label.clone())),
            }
            None => return Err(EvaluationError::Blame(blame_label.clone())),
        }


        
    }
    
    // Base rule
    Ok((new_env, Value::VCast(Box::new(result), type1.clone(), blame_label.clone(), type2.clone())))
    // Err(EvaluationError::NotYetImplemented(Term::Cast(Box::new(term.clone()), type1.clone(), blame_label.clone(), type2.clone())))
}

fn unwrap_vcast_variant(variant: &Value) -> (String, Value) {
    match variant {
        Value::VVariant(label, val) => (label.clone(), *val.clone()),
        Value::VCast(sub, _, _, _) => unwrap_vcast_variant(&**sub),
        _ => panic!("Error in evaluate_cast")
    }
}

pub fn evaluate_convertion(env: Environment, term: &Term, type1: &Type, convertion_label: &ConvertionLabel, type2: &Type) -> Eval {
    let type1 = &type1.normalize();
    let type2 = &type2.normalize();
    let (new_env, result) = evaluate(env, term)?;

    // ------------------------------------------------------- A is a base type
    // rho |-conv v : A ~[-alpha]~> alpha --> VSeal v A alpha
    if !convertion_label.is_present() && type2.is_type_name() && type1.is_base_type() {
        let t2_alpha = match type2.clone() {
            Type::TypeName(alpha) => alpha,
            _ => panic!("evaluate_convertion: Error in:  v : A ~[-alpha]~> alpha --> VSeal v A alpha")
        };

        return Ok((new_env, Value::VSeal(Box::new(result.clone()), type1.clone(), t2_alpha)));
    }

    // rho |-conv (VSeal v A alpha) : alpha ~[+alpha] A  -->  v
    match result.clone() {
        Value::VSeal(value, a, alpha) => match type1.clone() {
            Type::TypeName(lab) => match convertion_label.clone() {
                ConvertionLabel::Present(lab2) => if lab == alpha && lab == lab2 && &a == type2 {return Ok((new_env, *value))},
                _ => {}
            },
            _ => {}
        },
        _ => {}
    }

    // rho |-conv v : * ~[+/-alpha]~> *  -->  v
    if type1 == type2 && type1.is_dyn() {return Ok((new_env, result))}

    //  -------------------------------------------------- if alpha != beta
    //  rho |-conv v : alpha ~[+/-beta]~> alpha --> v
    if type1 == type2 {
        match type1.clone() {
            Type::TypeName(lab) => if lab != convertion_label.to_label() {return Ok((new_env, result))},
            _ => {}
        }
    }

    //  -------------------------------------------------- if A is a base type
    //  rho |-conv v : A ~[+/-alpha]~> A --> v
    if type1 == type2 && type1.is_base_type() {return Ok((new_env, result))}


    // rho |-conv v : (A1 -> B1) ~[+/-alpha]~> (A2 -> B2) 
    //    --> lam (x:A2) (v (x : A2 ~[-/+alpha]~> A1) : B1 ~[+/-alpha]~> B2)
    // v is a function here so (v (x .. is a function application

    match type1.clone() {
        Type::FunctionType(a1, b1) => match type2.clone() {
            Type::FunctionType(a2, b2) => {
                // println!("{:?}", term);
                let argument = Term::Convertion(Box::new(Term::Variable("__internal".to_string())), *a2.clone(), convertion_label.clone().invert(), *a1.clone());
                // let convertion_body= Term::FunctionCall(Box::new(Term::Constant(result)), Box::new(argument));
                let convertion_body= Term::FunctionCall(Box::new(term.clone()), Box::new(argument));
                let lambda_body = Term::Convertion(Box::new(convertion_body), *b1, convertion_label.clone(), *b2.clone());
                let lambda = Term::AnonymousFunction("__internal".to_string(), *a2.clone(), *b2.clone(), Box::new(lambda_body));

                return evaluate(new_env, &lambda)
            },
            _ => {},
        },
        // rho |-conv v : (all <X:K> A1) ~[+/-alpha]~> (all <X:K> A2)
        //  --> lam <X:K> (v <X> : A1 ~[+/-alpha]~> A2)
        Type::UniversalType(typevar1, kind1, a1) => match type2.clone() {
            Type::UniversalType(typevar2, kind2, a2) => {
                if typevar1 == typevar2 && kind1 == kind2 {
                    let convertion_body = Term::TypeApplication(Box::new(Term::Constant(result)), Type::TypeVariable(typevar1.clone()));
                    let big_lam_body = Term::Convertion(Box::new(convertion_body), *a1, convertion_label.clone(), *a2.clone());
                    let big_lam = Term::BigLambda(typevar1, kind1, Box::new(big_lam_body));
                    // let big_lam = Term::BigLambda(typevar1, kind1, Box::new(big_lam_body), *a2.clone());
                    return evaluate(new_env, &big_lam)
                }
            },
            _ => {}
        },
        _ => {}
    }

    Ok((new_env, Value::VSealPostConv(Box::new(result), type1.clone(), convertion_label.clone(), type2.clone())))
}

pub fn evaluate_logic(env: Environment, logic_term: &LogicTerm) -> Eval {
    fn map_logic_result(env: Environment, preliminary_result: Result<Value, InvalidValueTranslation>) -> Eval {
        match preliminary_result {
            Ok(value) => Ok((env, value)),
            Err(err) => Err(EvaluationError::LogicError(err)),
        }
    }

    macro_rules! match_helper {
        ($func:ident, $env:ident, $block1:ident, $block2:ident) => {
            {
                let (env1, res1) = evaluate($env, $block1)?;
                let (env2, res2) = evaluate(env1, $block2)?;
                let (res1, res2, ground_type, blame) = unwrap_dynamics(res1, res2);
                match map_logic_result(env2, res1.$func(res2)) {
                    Ok((env3, res3)) => if ground_type.is_none() {Ok((env3, res3))} else {Ok((env3, Value::VDynamic(Box::new(res3), ground_type.expect("Error in evaluation: evaluate_logic"), blame.expect("Error in evaluation: evaluate_logic"))))},
                    Err(err) => {
                        match err {
                            EvaluationError::LogicError(err) => Err(EvaluationError::ExtendedLogicError($block1.clone(), err, $block2.clone())),
                            _ => Err(err),
                        }
                    },
                }
            }
        }
    }

    match logic_term {
        LogicTerm::Not(block) => {
            let (env, result) = evaluate(env, block)?;
            map_logic_result(env, result.negate())
        }
        ,
        LogicTerm::And(block1, block2) => {
            match evaluate(env, block1) {
                Ok((env, Value::VBool(true))) => evaluate(env, block2),
                Ok((env, Value::VBool(false))) => Ok((env, Value::VBool(false))),
                Ok((_, _)) => Err(EvaluationError::UnknownLogicError),
                Err(err) => Err(err),
            }
        },
        LogicTerm::Or(block1, block2) => {
            match evaluate(env, block1) {
                Ok((env, Value::VBool(false))) => evaluate(env, block2),
                Ok((env, Value::VBool(true))) => Ok((env, Value::VBool(true))),
                Ok((_, _)) => Err(EvaluationError::UnknownLogicError),
                Err(err) => Err(err),
            }
        },
        LogicTerm::Add(block1, block2) => {
            match_helper!(add, env, block1, block2)
        },
        LogicTerm::Sub(block1, block2) => {
            match_helper!(sub, env, block1, block2)
        },
        LogicTerm::Mul(block1, block2) => {
            match_helper!(mul, env, block1, block2)
        },
        LogicTerm::Div(block1, block2) => {
            match_helper!(div, env, block1, block2)
        },
        LogicTerm::Eql(block1, block2) => {
            match_helper!(eql, env, block1, block2)
        },
        LogicTerm::GrT(block1, block2) => {
            match_helper!(grt, env, block1, block2)
        },
        LogicTerm::LsT(block1, block2) => {
            match_helper!(lst, env, block1, block2)
        },
        LogicTerm::GrE(block1, block2) => {
            match_helper!(gre, env, block1, block2)
        },
        LogicTerm::LsE(block1, block2) => {
            match_helper!(lse, env, block1, block2)
        },
    }
}

