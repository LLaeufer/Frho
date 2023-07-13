use crate::interpreter_environment::*;
use crate::interpreter_evaluation::*;
use crate::interpreter_utils::*;

use std::collections::HashSet;
use std::collections::HashMap;
use std::result;
use std::sync::Arc;




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
    let desired = Ok((desired_env, Value::VNone));
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
fn test_function_call() {
    let env = Environment::new();
    let desired_env = Environment::new();
    let func_term_block = Term::Block(Arc::new(vec![Term::Function("addFunc".to_string(), vec!["a".to_string(), "b".to_string()], Box::new(Term::Block(Arc::new(vec![
        Term::LogicGate(LogicTerm::Add(Box::new(Term::Variable("a".to_string())), Box::new(Term::Variable("b".to_string()))))])))),
        Term::FunctionCall(Box::new(Term::Variable("addFunc".to_string())), vec![Term::Constant(Value::VInt(40)), Term::Constant(Value::VInt(2))])
        ]));
    let result = evaluate(env, &func_term_block);
    let desired = Ok((desired_env, Value::VInt(42)));
    assert_equal_evaluation(result, desired)
}

#[allow(dead_code)]
pub fn test_function_call_recursion_abst(count: i32) {
    let env = Environment::new();
    let desired_env = Environment::new();
    let func_term_block = Term::Block(Arc::new(vec![Term::Function("count".to_string(), vec!["a".to_string()], Box::new(Term::If(Box::new(Term::LogicGate(LogicTerm::Eql(Box::new(Term::Variable("a".to_string())), Box::new(Term::Constant(Value::VInt(count)))))), Box::new(Term::Variable("a".to_string())), Box::new(
        Term::FunctionCall(Box::new(Term::Variable("count".to_string())), vec![Term::LogicGate(LogicTerm::Add(Box::new(Term::Variable("a".to_string())), Box::new(Term::Constant(Value::VInt(1)))))]))))
    ),

        Term::FunctionCall(Box::new(Term::Variable("count".to_string())), vec![Term::Constant(Value::VInt(0))])
        ]));
    let result = evaluate(env, &func_term_block);
    println!("{:?}", result);
    let desired = Ok((desired_env, Value::VInt(count)));
    assert_equal_evaluation(result, desired)
}

#[test]
fn test_function_call_recursion() {
    test_function_call_recursion_abst(250);
}



#[test]
fn test_function_call_recursion_2() {
    use std::thread;
    const STACK_SIZE: usize = 4 * 1024 * 1024 * 1024;
    fn run() {
        test_function_call_recursion_abst(250_000);
    }

    let child = thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(run)
        .unwrap();

    // Wait for thread to join
    child.join().unwrap();
}


#[test]
fn test_anonymous_function_call() {
    let env = Environment::new();
    let desired_env = Environment::new();
    let func_term_block = Term::Block(Arc::new(vec![Term::Let("addFunc".to_string(), Box::new(Term::AnonymousFunction(vec!["a".to_string(), "b".to_string()], Box::new(
        Term::LogicGate(LogicTerm::Add(Box::new(Term::Variable("a".to_string())), Box::new(Term::Variable("b".to_string())))))))) ,
        Term::FunctionCall(Box::new(Term::Variable("addFunc".to_string())), vec![Term::Constant(Value::VInt(40)), Term::Constant(Value::VInt(2))])
        ]));
    let result = evaluate(env, &func_term_block);
    let desired = Ok((desired_env, Value::VInt(42)));
    assert_equal_evaluation(result, desired)
}


#[test]
fn test_anonymous_function_call_2() {
    let env = Environment::new();
    let desired_env = Environment::new();
    let func_term_block = 
        Term::FunctionCall(Box::new(Term::AnonymousFunction(vec!["a".to_string(), "b".to_string()], Box::new(
            Term::LogicGate(LogicTerm::Add(Box::new(Term::Variable("a".to_string())), Box::new(Term::Variable("b".to_string()))))))), vec![Term::Constant(Value::VInt(40)), Term::Constant(Value::VInt(2))]);
    let result = evaluate(env, &func_term_block);
    let desired = Ok((desired_env, Value::VInt(42)));
    assert_equal_evaluation(result, desired)
}


#[test]
fn test_record_construction() {
    let env = Environment::new();
    let desired_env = Environment::new();
    let record_const = Term::RecordConstruction(vec![("good_number".to_string(), Box::new(Term::Constant(Value::VInt(1337)))), ("best_number".to_string(), Box::new(Term::Block(Arc::new(vec![
        Term::FunctionCall(Box::new(Term::AnonymousFunction(vec!["a".to_string(), "b".to_string()], Box::new(
            Term::LogicGate(LogicTerm::Add(Box::new(Term::Variable("a".to_string())), 
            Box::new(Term::Variable("b".to_string()))))))), vec![Term::Constant(Value::VInt(40)), 
            Term::Constant(Value::VInt(2))])
        ]))))]);
    let result = evaluate(env, &record_const);
    let mut map = HashMap::new();
    map.insert("good_number".to_string(), Value::VInt(1337));
    map.insert("best_number".to_string(), Value::VInt(42));
    let desired = Ok((desired_env, Value::VRecord(map)));
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
    let record_select = Term::RecordSelection(Box::new(Term::Variable("rec".to_string())), 
        Box::new(Term::Constant(Value::VLabel("best_number".to_string()))));
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
        Box::new(Term::Constant(Value::VLabel("best_number".to_string()))),
        Box::new(Term::Constant(Value::VInt(42)))
    );
    let result = evaluate_single_term(env, &record_update);
    let desired = Ok((desired_env, Value::VRecord(map.clone())));
    assert_equal_evaluation(result, desired)
}

#[test]
fn test_variant_construction() {
    let mut env = Environment::new();
    let mut desired_env = Environment::new();
    let variant_const = Term::VariantConstruction(("best_number".to_string(), Box::new(Term::Constant(Value::VInt(42)))));
    let result = evaluate_single_term(env, &variant_const);
    let desired = Ok((desired_env, Value::VVariant("best_number".to_string(), Box::new(Value::VInt(42)))));
    assert_equal_evaluation(result, desired)
}

#[test]
fn test_type_application() {
    let mut env = Environment::new();
    let mut desired_env = Environment::new();
    let type_appl = Term::TypeApplication(Box::new(Term::Constant(Value::VAll(Box::new(Value::VInt(42))))), Type::IntType);
    let result = evaluate_single_term(env, &type_appl);
    // println!("{:?}", result);
    let desired = Ok((desired_env, Value::VInt(42)));
    assert_equal_evaluation(result, desired);
}