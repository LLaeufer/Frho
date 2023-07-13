use crate::interpreter_environment::*;
use crate::interpreter_utils::*;

use std::collections::HashMap;

#[derive(PartialEq, Debug, Clone)]
pub enum EvaluationError {
    NotYetImplemented(Term),
    VariableNotInEnvironment(String),
    VariableNotOfDesiredType(Value, Type),
    VariableCannotBeReassignedInTheSameBlock(String),
    LogicError(InvalidValueTranslation),
    UnknownLogicError,
    UnknownError,
}

pub type Eval = Result<(Environment, EvaluationResult), EvaluationError>;
pub type EvalResult = Result<EvaluationResult, EvaluationError>;
pub type CustomEval<T> = Result<(Environment, T), EvaluationError>;
pub type CustomEvalResult<T> = Result<T, EvaluationError>;

pub fn evaluate_single_term(env: Environment, term: &Term) -> Eval {
    match term {
        Term::Constant(value) => Ok((env, value.clone())),
        Term::Variable(var) => evaluate_variable(env, var),
        Term::DefineTypeVariable(var, types) => evaluate_define_type_variable(env, var, types),
        Term::LogicGate(logic_term) => evaluate_logic(env, logic_term),
        Term::Let(var, let_term) => evaluate_let(env, var, let_term),
        Term::If(decider, consequence, alternative) => evaluate_if(env, decider, consequence, alternative),
        Term::Function(label, parameters, body) => evaluate_function(env, label, parameters, body),
        Term::FunctionCall(func, parameter_inputs) => evaluate_function_call(env, func, parameter_inputs),
        Term::AnonymousFunction(parameters, parameter_inputs) => evaluate_anonymous_function(env, parameters, parameter_inputs),
        Term::RecordUpdate(record, label, new_value) => evaluate_record_update(env, record, label, new_value),
        Term::RecordSelection(record, label) => evaluate_record_selection(env, record, label),
        Term::RecordConstruction(variants) => evaluate_record_construction(env, variants),
        Term::VariantConstruction(variant) => evaluate_variant_construction(env, variant),
        Term::VariantCase(v_block, des_label, con_var, con, alt_var, alt) => evaluate_variant_case(env, v_block, des_label, con_var, con, alt_var, alt),
        Term::TypeApplication(term_block, types) => evaluate_type_application(env, term_block, types),
        Term::BigLambda(types, value) => evaluate_big_lambda(env, types, value),
        _ => Err(EvaluationError::NotYetImplemented(term.clone())),
    }
}

pub fn evaluate_variable(env: Environment, var: &String) -> Eval {
    let result = match env.get(var) {
        Some(value) => value.clone(),
        None => return Err(EvaluationError::VariableNotInEnvironment(var.clone()))
    };
    Ok((env, result))
}

pub fn evaluate_define_type_variable(env: Environment, var: &String, types: &Type) -> Eval {
    let mut env = env;
    env.insert_type(var.clone(), types.clone());
    Ok((env, Value::VNone))
}

pub fn evaluate_let(env: Environment, var: &String, result_term: &Term) -> Eval {
    let (mut new_env, result) = evaluate_single_term(env, result_term)?;
    if new_env.insert(var.clone(), result) {
        return Ok((new_env, Value::VNone));
    } else {Err(EvaluationError::VariableCannotBeReassignedInTheSameBlock(var.clone()))}
}

fn evaluate_block_rec(env: Environment, result: EvaluationResult, terms: &TermBlock, index: usize) -> Eval {
    if terms.len() == index { return Ok((env, result)) }
    let (new_env, new_result) = evaluate_single_term(env, &terms[index])?;
    evaluate_block_rec(new_env, new_result, terms, index+1)
}

pub fn evaluate(env: Environment, terms: &Term) -> Eval {
    // This new_child call leaks memory
    let (_, result) = match terms {
        Term::Block(block) => evaluate_block_rec(env.new_child(), Value::VNone, block, 0)?,
        _ => evaluate_single_term(env.new_child(), terms)?,
    };
    // We want to forget the block environment here and reuse the last environment
    Ok((env, result))
}

pub fn evaluate_function_block(env: &Environment, terms: &Term, parameter: HashMap<Var, Value>) -> EvalResult {
    let (_, result) = match terms {
        Term::Block(block) => evaluate_block_rec(env.new_child_with(parameter), Value::VNone, block, 0)?,
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
        _ => Err(EvaluationError::UnknownLogicError),
    }
}

pub fn evaluate_function(env: Environment, label: &Label, parameters: &Vec<Var>, body: &Term) -> Eval {
    let mut mut_env = env;
    let func_env = mut_env.new_child();
    if mut_env.insert(label.clone(), Value::VFunc(Box::new(func_env), parameters.clone(), Box::new(body.clone()))) {
        return Ok((mut_env, Value::VNone)); 
    }
    Err(EvaluationError::VariableCannotBeReassignedInTheSameBlock(label.clone()))
}

pub fn evaluate_anonymous_function(env: Environment, parameters: &Vec<Var>, body: &Term) -> Eval {
    let func_env = env.new_child();
    Ok((env, Value::VFunc(Box::new(func_env), parameters.clone(), Box::new(body.clone()))))
} 

fn evaluate_block_vector(env: Environment, block_vector: &Vec<Term>) -> CustomEval<Vec<EvaluationResult>> {
    fn eval_b_v_internal(env: Environment, b_v: &Vec<Term>, res: Vec<EvaluationResult>) -> CustomEval<Vec<EvaluationResult>> {
        if b_v.len() == res.len() {
            return Ok((env, res));
        }
        let (eval_env, eval_result) = evaluate(env, &b_v[res.len()])?;
        let mut mut_res = res;
        mut_res.push(eval_result);
        eval_b_v_internal(eval_env, b_v, mut_res)
    }
    eval_b_v_internal(env, block_vector, vec![])

}

pub fn map_from_parameter(vars: Vec<Var>, values: Vec<Value>) -> HashMap<Var, Value> {
    std::iter::zip(vars, values).collect()
}

pub fn evaluate_function_call(env: Environment, func_body_location: &Term, parameter_values: &Vec<Term>) -> Eval {
    // Evaluate all inputs for the function call later
    let (env, param_results) = evaluate_block_vector(env, parameter_values)?;
    // Search for value matching the function name
    let (env, maybe_func) = evaluate(env, func_body_location)?;
        // Test whether value is a function
    match maybe_func {
        // Execute the function
        Value::VFunc(func_env, params, body) => 
            Ok((env, evaluate_function_block(&func_env, &body, 
                map_from_parameter(params.clone(), param_results))?)),
        // Value isn't a function
        _ => Err(EvaluationError::VariableNotOfDesiredType(maybe_func, Type::FunctionType(vec![], vec![])))
    }
}

pub fn evaluate_record_update(env: Environment, record: &Term, label: &Term, new_value: &Term) -> Eval {
    let (env, record_result) = evaluate(env, record)?;
    let (env, label_result) = evaluate(env, label)?;
    let (env, new_value_result) = evaluate(env, new_value)?;
    match record_result {
        Value::VRecord(map) => {
            let mut new_map = map.clone();
            match label_result {
                Value::VLabel(lab) =>{
                    new_map.insert(lab, new_value_result);
                    Ok((env, Value::VRecord(new_map)))
                }
                _ => Err(EvaluationError::VariableNotOfDesiredType(label_result, Type::LabelType))
            }
        },
        _ => Err(EvaluationError::VariableNotOfDesiredType(record_result, Type::RecordsType(vec![])))
    }
}

pub fn evaluate_record_selection(env: Environment, record: &Term, label: &Term) -> Eval {
    let (env, record_result) = evaluate(env, record)?;
    let (env, label_result) = evaluate(env, label)?;
    match record_result {
        Value::VRecord(map) => {
            match label_result {
                Value::VLabel(lab) =>{
                    Ok((env, map[&lab].clone()))
                }
                _ => Err(EvaluationError::VariableNotOfDesiredType(label_result, Type::LabelType))
            }
        },
        _ => Err(EvaluationError::VariableNotOfDesiredType(record_result, Type::RecordsType(vec![])))
    }
}

//pub fn tuple_to_map(tup: Vec<(Var, Value)>) -> HashMap<Var, Value> {
//    tup.into_iter().collect()
//}

pub fn evaluate_record_construction(env: Environment, unevaluated_record: &Vec<RawVariant>) -> Eval {
    fn eval_rec_con_rec(env: Environment, un_rec: &Vec<RawVariant>, mut rec: Vec<Variant>) -> CustomEval<Vec<Variant>> {
        if un_rec.len() == rec.len() { return Ok((env, rec)); }
        let (label, terms) = &un_rec[rec.len()];
        let (env, result) = evaluate(env, &terms)?;
        rec.push((label.clone(), result));
        eval_rec_con_rec(env, un_rec, rec)
    }

    let (env, variants) = eval_rec_con_rec(env, &unevaluated_record, vec![])?;
    let map = tuple_to_map(variants);
    Ok((env, Value::VRecord(map)))

}

pub fn evaluate_variant_construction(env: Environment, unevaluated_variant: &RawVariant) -> Eval {
    let (variant_label, variant_term) = unevaluated_variant; 
    let (env, variant_result) = evaluate(env, &variant_term)?;
    Ok((env, Value::VVariant(variant_label.clone(), Box::new(variant_result))))
}

pub fn evaluate_variant_case(env: Environment, variant_block: &Term, desired_label: &Label, consequence_var: &Var, consequence: &Term, alternative_var: &Var, alternative: &Term) -> Eval {
    let (env, maybe_variant) = evaluate(env, variant_block)?;
    match maybe_variant {
        Value::VVariant(label, value) => if &label == desired_label {
            let result = evaluate_function_block(&env, &consequence, tuple_to_map(vec![(consequence_var.clone(), *value.clone())]))?;
            Ok((env, result))
        } else {
            let result = evaluate_function_block(&env, &alternative, tuple_to_map(vec![(alternative_var.clone(), Value::VVariant(label.clone(), value.clone()))]))?;
            Ok((env, result))
        },
        _ => Err(EvaluationError::VariableNotOfDesiredType(maybe_variant, Type::VariantType(vec![])))
    }
}

pub fn evaluate_type_application(env: Environment, block: &Term, types: &Type) -> Eval {
    let (env, block_result) = evaluate(env, block)?;
    match block_result.cast_type(types) {
        Ok(casted) => Ok((env, casted)),
        Err(err) => Err(EvaluationError::VariableNotOfDesiredType(block_result, types.clone())),
    }
}

pub fn evaluate_big_lambda(env: Environment, types: &Type, value: &Value) -> Eval {
    Ok((env, Value::VAll(Box::new(value.clone()))))
    // Hier fehlt noch das die typen richtig behandelt werden. Leider verstehe ich das noch nicht ganz von der Definition
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
                map_logic_result(env2, res1.$func(res2))
            }
        };
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

