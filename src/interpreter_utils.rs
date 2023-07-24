use crate::interpreter_environment::*;
use crate::interpreter_evaluation::*;

use std::collections::HashSet;
use std::collections::HashMap;

pub fn tuple_to_map(tup: Vec<(Var, Value)>) -> HashMap<Var, Value> {
    tup.into_iter().collect()
}

pub fn string_to_str(input: &String) -> &str {
    let slice: &str = &*input;
    slice
}


pub fn assert_equal_evaluation(result: Eval, desired: Eval) {
    match result {
        Ok((env, value)) => match desired {
            Ok((desired_env, desired_value)) => {
                assert!(assert_equal_result(value, desired_value));
                assert!(assert_equal_environments(env, desired_env));
            },
            Err(_) => assert!(false),
        },
        Err(err) => match desired {
            Ok(_) => assert!(false),
            Err(desired_err) => assert!(assert_equal_error(err, desired_err)),
        },
    }
}

pub fn assert_equal_result(result_value: Value, desired_value: Value) -> bool {
    result_value == desired_value
}

pub fn assert_equal_environments(result_env: Environment, desired_env: Environment) -> bool {
    let re_keys = result_env.keys();
    let de_keys = desired_env.keys();
    let diff: HashSet<String> = re_keys.difference(&de_keys).cloned().collect();
    if diff.is_empty() {
        for item in re_keys.iter() {
            if result_env.get(item) != desired_env.get(item) { return false; }
        }
        true
    } else {
        false
    }
}

pub fn assert_equal_error(result_err: EvaluationError, desired_err: EvaluationError) -> bool {
    result_err == desired_err
}