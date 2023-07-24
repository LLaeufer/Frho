use crate::parser_grammar::*;
use crate::interpreter_environment::*;
use crate::interpreter_utils::*;

use crate::interpreter_evaluation::*;
use crate::interpreter::*;

use std::collections::HashMap;
use std::sync::Arc;


#[test]
fn test_recursion() {
    let parsed = CodeParser::new().parse(r"
        FUN count (a: INT) (
            IF (a == 100) (a) ELSE (CALL count(a+1))
        );
        CALL count(0)
    ");
    assert!(parsed.is_ok());
    match parsed {
        Ok(cparsed) => {
            let result = evaluate(Environment::new(), &cparsed);
            let desired_env = Environment::new();
            let desired = Ok((desired_env, Value::VInt(100)));
            assert_equal_evaluation(result, desired)
        },
        Err(_) => todo!(),
    }

    // More minimal syntax
    let parsed = CodeParser::new().parse(r"
        FUN count (a: INT) (
            IF (a == 100) a ELSE CALL count(a+1)
        );
        CALL count(0)
    ");
    assert!(parsed.is_ok());
    match parsed {
        Ok(cparsed) => {
            let result = evaluate(Environment::new(), &cparsed);
            let desired_env = Environment::new();
            let desired = Ok((desired_env, Value::VInt(100)));
            assert_equal_evaluation(result, desired)
        },
        Err(_) => todo!(),
    }

    // Super minimal syntax
    let parsed = CodeParser::new().parse(r"
        FUN count (a: INT) IF (a == 100) a ELSE CALL count(a+1);
        CALL count(0)
    ");
    assert!(parsed.is_ok());
    match parsed {
        Ok(cparsed) => {
            let result = evaluate(Environment::new(), &cparsed);
            let desired_env = Environment::new();
            let desired = Ok((desired_env, Value::VInt(100)));
            assert_equal_evaluation(result, desired)
        },
        Err(_) => todo!(),
    }




}
fn test_helper(path: String) -> (Term, Value) { 
    let text = load_program_text(path);
    let ast = parse_program_text(text);
    let result = evaluate_program_high_stack_size(ast.clone());
    return (ast, result);
}

macro_rules! test_result {
    ($path:expr, $expected:expr) => {
        let (_, result) = test_helper($path.to_string());
        assert_eq!(result, $expected);
    };
}

macro_rules! test_sample {
    ($name:ident, $path:expr, $expected:expr) => {
        #[test]
        fn $name() {
            test_result!($path, $expected);
        }
    };
}

test_sample!(test_sample_case, "samples/case.frho", Value::VString("yay".to_string()));
test_sample!(test_sample_fib, "samples/fib.frho", Value::VInt(75025));
test_sample!(test_sample_let, "samples/let.frho", Value::VInt(42));
test_sample!(test_sample_records, "samples/records.frho", Value::VString("piggy".to_string()));
test_sample!(test_sample_recursion, "samples/recursion.frho", Value::VInt(100));
test_sample!(test_sample_recursion100k, "samples/recursion100k.frho", Value::VInt(100_000));
test_sample!(test_sample_wrap_and_unwrap, "samples/wrap_and_unwrap.frho", Value::VInt(7));
test_sample!(test_sign_presidence_1, "samples/sign-presidence/1.frho", Value::VInt(198));
test_sample!(test_sign_presidence_2, "samples/sign-presidence/2.frho", Value::VBool(false));
test_sample!(test_sign_presidence_3, "samples/sign-presidence/3.frho", Value::VBool(true));
test_sample!(test_sample_test_leak, "samples/test_leak.frho", Value::VInt(10));
test_sample!(test_sample_test_leak_2, "samples/test_leak_2.frho", Value::VInt(11));
test_sample!(test_sample_test_leak_3, "samples/test_leak_3.frho", Value::VInt(1));


#[test]
fn test_sample_record_self_assign () {
    let mut inner_map = HashMap::new();
    inner_map.insert("test".to_string(), Value::VRecord(HashMap::new()));
    let mut map = HashMap::new();
    map.insert("test".to_string(), Value::VRecord(inner_map));
    test_result!("samples/record_self_assign.frho", Value::VRecord(map));
}
#[test]
fn test_sample_wrap_and_unwrap_2 () {
    let mut map = HashMap::new();
    map.insert("pig".to_string(), Value::VString("oink".to_string()));
    map.insert("number".to_string(), Value::VInt(42));
    test_result!("samples/wrap_and_unwrap_2.frho", Value::VRecord(map));
}

#[test]
fn test_return_function () {
    let mut env = Environment::new();
    let ast = Term::If(Box::new(Term::Block(Arc::new(vec![Term::LogicGate(LogicTerm::LsE(Box::new(Term::Variable("a".to_string())), Box::new(Term::Constant(Value::VInt(2)))))]))), Box::new(Term::Constant(Value::VInt(1))), 
        Box::new(Term::Block(Arc::new(vec![Term::LogicGate(LogicTerm::Add(Box::new(Term::FunctionCall(Box::new(Term::Variable("fib".to_string())), vec![Term::LogicGate(LogicTerm::Sub(Box::new(Term::Variable("a".to_string())), Box::new(Term::Constant(Value::VInt(1)))))])), Box::new(Term::FunctionCall(Box::new(Term::Variable("fib".to_string())), vec![Term::LogicGate(LogicTerm::Sub(Box::new(Term::Variable("a".to_string())), Box::new(Term::Constant(Value::VInt(2)))))]))))]))));
    let desired = Value::VFunc(Box::new(env.new_child()), vec![("a".to_string(), Type::IntType)], Box::new(ast));
    env.insert("fib".to_string(), desired.clone());
    let (_ast, result) = test_helper("samples/return_function.frho".to_string());
    match result {
        Value::VFunc(env, params , block) => match desired {
            Value::VFunc(denv, dparams, dblock)=> {
                assert_eq!(env.keys(), denv.keys());
                assert_eq!(params, dparams);
                assert_eq!(block, dblock);
            },
            _ => assert!(false),
        },
        _ => assert!(false),
    }
}

