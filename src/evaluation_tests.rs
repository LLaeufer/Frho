use crate::evaluation::*;
use crate::environment::*;
use crate::utils::*;
use crate::values::*;
use crate::terms::*;
use crate::logicterms::*;

use std::collections::HashMap;




#[test]
fn test_term_constant() {
    let env = Environment::new();
    let result = evaluate_single_term(env, &Term::Constant(Value::VInt(42)));
    let empty_env = Environment::new();
    let desired = Ok((empty_env, Value::VInt(42)));
    assert_equal_evaluation(result, desired)
}

#[test]
fn test_term_let() {
    let env = Environment::new();
    let let_term = Term::Let("fortytwo".to_string(), Box::new(Term::Constant(Value::VInt(42))));
    let result = evaluate_single_term(env, &let_term);
    let mut desired_env = Environment::new();
    desired_env.insert("fortytwo".to_string(), Value::VInt(42));
    let desired = Ok((desired_env, unit_value()));
    assert_equal_evaluation(result, desired)
}

#[test]
fn test_term_variable() {
    let mut env = Environment::new();
    env.insert("fortytwo".to_string(), Value::VInt(42));
    let variable_term = &Term::Variable("fortytwo".to_string());
    let result = evaluate_single_term(env, variable_term);

    let mut desired_env = Environment::new();
    desired_env.insert("fortytwo".to_string(), Value::VInt(42));
    let desired = Ok((desired_env, Value::VInt(42)));
    assert_equal_evaluation(result, desired);
}

#[test]
fn test_term_logicgate_not() {
    let env = Environment::new();
    let desired_env = Environment::new();
    let negate_term = & Term::LogicGate(LogicTerm::Not(Box::new(Term::Constant(Value::VBool(true)))));
    let result = evaluate_single_term(env, negate_term);
    let desired = Ok((desired_env, Value::VBool(false)));
    assert_equal_evaluation(result, desired);
}

#[test]
fn test_term_logicgate_and() {
    let env = Environment::new();
    let desired_env = Environment::new();
    let and_term = & Term::LogicGate(LogicTerm::And(Box::new(Term::Constant(Value::VBool(true))), Box::new(Term::Constant(Value::VBool(true)))));
    let result = evaluate_single_term(env, and_term);
    let desired = Ok((desired_env, Value::VBool(true)));
    assert_equal_evaluation(result, desired);
}

#[test]
fn test_term_logicgate_add() {
    let env = Environment::new();
    let desired_env = Environment::new();
    let add_term = & Term::LogicGate(LogicTerm::Add(Box::new(Term::Constant(Value::VInt(40))), Box::new(Term::Constant(Value::VInt(2)))));
    let result = evaluate_single_term(env, add_term);
    let desired = Ok((desired_env, Value::VInt(42)));
    assert_equal_evaluation(result, desired);
}

#[test]
fn test_term_logicgate_sub() {
    let env = Environment::new();
    let desired_env = Environment::new();
    let add_term = & Term::LogicGate(LogicTerm::Sub(Box::new(Term::Constant(Value::VInt(44))), Box::new(Term::Constant(Value::VInt(2)))));
    let result = evaluate_single_term(env, add_term);
    let desired = Ok((desired_env, Value::VInt(42)));
    assert_equal_evaluation(result, desired);
}

#[test]
fn test_term_logicgate_or() {
    let env = Environment::new();
    let desired_env = Environment::new();
    let or_term = & Term::LogicGate(LogicTerm::Or(Box::new(Term::Constant(Value::VBool(false))), Box::new(Term::Constant(Value::VBool(true)))));
    let result = evaluate_single_term(env, or_term);
    let desired = Ok((desired_env, Value::VBool(true)));
    assert_equal_evaluation(result, desired);
}

#[test]
fn test_term_if() {
    let env = Environment::new();
    let desired_env = Environment::new();
    let if_term = &Term::If(Box::new(Term::Constant(Value::VBool(true))), 
                                   Box::new(Term::Constant(Value::VString("consequence".to_string()))), 
                                   Box::new(Term::Constant(Value::VString("alternative".to_string()))));
    let result = evaluate_single_term(env, if_term);
    let desired = Ok((desired_env, Value::VString("consequence".to_string())));
    assert_equal_evaluation(result, desired)
}

#[test]
fn test_term_if_alternative() {
    let env = Environment::new();
    let desired_env = Environment::new();
    let if_term = &Term::If(Box::new(Term::Constant(Value::VBool(false))), 
                                   Box::new(Term::Constant(Value::VString("consequence".to_string()))), 
                                   Box::new(Term::Constant(Value::VString("alternative".to_string()))));
    let result = evaluate_single_term(env, if_term);
    let desired = Ok((desired_env, Value::VString("alternative".to_string())));
    assert_equal_evaluation(result, desired)
}

#[test]
fn test_record_selection() {
    let mut env = Environment::new();
    let mut desired_env = Environment::new();
    let mut map = HashMap::new();
    map.insert("good_number".to_string(), Value::VInt(1337));
    map.insert("best_number".to_string(), Value::VInt(42));
    env.insert("rec".to_string(), Value::VRecord(map.clone()));
    desired_env.insert("rec".to_string(), Value::VRecord(map.clone()));
    let record_select = Term::RecordSelection(Box::new(Term::Variable("rec".to_string())), "best_number".to_string());
    let result = evaluate_single_term(env, &record_select);
    let desired = Ok((desired_env, Value::VInt(42)));
    assert_equal_evaluation(result, desired)
}

#[test]
fn test_record_update() {
    let mut env = Environment::new();
    let mut desired_env = Environment::new();
    let mut map = HashMap::new();
    map.insert("good_number".to_string(), Value::VInt(1337));
    map.insert("best_number".to_string(), Value::VInt(43));
    env.insert("rec".to_string(), Value::VRecord(map.clone()));
    desired_env.insert("rec".to_string(), Value::VRecord(map.clone()));
    map.insert("best_number".to_string(), Value::VInt(42));
    let record_update = Term::RecordUpdate(
        Box::new(Term::Variable("rec".to_string())), 
        "best_number".to_string(),
        Box::new(Term::Constant(Value::VInt(42)))
    );
    let result = evaluate_single_term(env, &record_update);
    let desired = Ok((desired_env, Value::VRecord(map.clone())));
    assert_equal_evaluation(result, desired)
}

#[test]
fn test_variant_construction() {
    let env = Environment::new();
    let desired_env = Environment::new();
    let variant_const = Term::VariantConstruction(("best_number".to_string(), Box::new(Term::Constant(Value::VInt(42)))));
    let result = evaluate_single_term(env, &variant_const);
    let desired = Ok((desired_env, Value::VVariant("best_number".to_string(), Box::new(Value::VInt(42)))));
    assert_equal_evaluation(result, desired)
}