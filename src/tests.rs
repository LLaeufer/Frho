use crate::parser_grammar::*;
use crate::environment::*;
use crate::utils::*;
use crate::types::*;
use crate::values::*;
use crate::terms::*;
use crate::logicterms::*;

use crate::evaluation::*;
use crate::interpreter::*;

use std::collections::HashMap;
use std::sync::Arc;


#[test]
fn test_recursion() {
    let parsed = CodeParser::new().parse(r"
        fun count (a: INT) (
            if (a == 100) then (a) else (count(a+1))
        );
        count(0)
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
        let (ast, result) = test_helper($path.to_string());
        let reconstructed = reconstruct_source_from_ast(&ast);
        let recon_ast = parse_program_text(reconstructed);
        assert_eq!(result, $expected);
        assert_eq!(ast, recon_ast);
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

test_sample!(test_sample_fac, "samples/fac.frho", Value::VInt(120));
test_sample!(test_sample_fib_anon_rec, "samples/fib_anon_rec.frho", Value::VInt(75025));
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
test_sample!(test_sample_test_leak_5, "samples/test_leak_5.frho", Value::VInt(11));
test_sample!(test_sample_test_leak_6, "samples/test_leak_6.frho", Value::VInt(5));
test_sample!(test_sample_test_leak_7, "samples/test_leak_7.frho", Value::VInt(5));
test_sample!(test_sample_test_leak_8_a, "samples/test_leak_8_a.frho", Value::VInt(5));
test_sample!(test_sample_test_leak_8_b, "samples/test_leak_8_b.frho", Value::VInt(5));
test_sample!(test_sample_test_leak_8_c, "samples/test_leak_8_c.frho", Value::VInt(5));
test_sample!(test_sample_test_leak_8_d, "samples/test_leak_8_d.frho", Value::VInt(5));
test_sample!(test_sample_test_leak_8_e, "samples/test_leak_8_e.frho", Value::VInt(5));
test_sample!(test_term_checks_logicgate_possible_operation, "typecheck-samples/term-checks/logicgate/possible-operation.frho", Value::VString("Hello World!".to_string()));
test_sample!(test_term_checks_logicgate_possible_operation_2, "typecheck-samples/term-checks/logicgate/possible-operation-2.frho", Value::VFloat(7.0));
test_sample!(test_term_checks_logicgate_possible_operation_3, "typecheck-samples/term-checks/logicgate/possible-operation-3.frho", Value::VFloat(7.0));
test_sample!(test_sample_variant, "samples/variant.frho", Value::VInt(42));
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

/*
#[test]
fn test_return_function () {
    let mut env = Environment::new();
    let ast = Term::Block(Arc::new(vec![Term::Block(Arc::new(vec![Term::If(Box::new(Term::Block(Arc::new(vec![Term::LogicGate(LogicTerm::LsE(Box::new(Term::Variable("a".to_string())), Box::new(Term::Constant(Value::VInt(2)))))]))), Box::new(Term::Constant(Value::VInt(1))), 
        Box::new(Term::Block(Arc::new(vec![Term::LogicGate(LogicTerm::Add(Box::new(Term::FunctionCall(Box::new(Term::Variable("fib".to_string())), vec![Term::LogicGate(LogicTerm::Sub(Box::new(Term::Variable("a".to_string())), Box::new(Term::Constant(Value::VInt(1)))))])), Box::new(Term::FunctionCall(Box::new(Term::Variable("fib".to_string())), vec![Term::LogicGate(LogicTerm::Sub(Box::new(Term::Variable("a".to_string())), Box::new(Term::Constant(Value::VInt(2)))))]))))]))))]))]));
    let desired = Value::VFunc(Box::new(env.new_child()), vec![("a".to_string(), Type::IntType)], Box::new(ast));
    env.insert("fib".to_string(), desired.clone());
    let (_ast, result) = test_helper("samples/return_function.frho".to_string());
    match result {
        Value::VFunc(env, params , block) => match desired {
            Value::VFunc(denv, dparams, dblock) => {
                assert_eq!(env.keys(), denv.keys());
                assert_eq!(params, dparams);
                assert_eq!(block, dblock);
            },
            _ => assert!(false),
        },
        _ => assert!(false),
    }
}
*/

#[test]
fn test_return_function () {
    let mut env = Environment::new();
    let ast = Term::Block(Arc::new(vec![Term::Block(Arc::new(vec![Term::If(Box::new(Term::Block(Arc::new(vec![Term::LogicGate(LogicTerm::LsE(Box::new(Term::Variable("a".to_string())), Box::new(Term::Constant(Value::VInt(2)))))]))), Box::new(Term::Block(Arc::new(vec![Term::Constant(Value::VInt(1))]))), 
        Box::new(Term::Block(Arc::new(vec![Term::LogicGate(LogicTerm::Add(Box::new(Term::FunctionCall(Box::new(Term::Variable("fib".to_string())), vec![Term::LogicGate(LogicTerm::Sub(Box::new(Term::Variable("a".to_string())), Box::new(Term::Constant(Value::VInt(1)))))])), Box::new(Term::FunctionCall(Box::new(Term::Variable("fib".to_string())), vec![Term::LogicGate(LogicTerm::Sub(Box::new(Term::Variable("a".to_string())), Box::new(Term::Constant(Value::VInt(2)))))]))))]))))]))]));
    let desired = Value::VFunc(Box::new(env.new_child()), vec![("a".to_string(), Type::IntType)], Box::new(ast));
    env.insert("fib".to_string(), desired.clone());
    let (_ast, result) = test_helper("samples/return_function.frho".to_string());
    match result {
        Value::VFunc(env, params , block) => match desired {
            Value::VFunc(denv, dparams, dblock) => {
                assert_eq!(env.keys(), denv.keys());
                assert_eq!(params, dparams);
                assert_eq!(block, dblock);
            },
            _ => assert!(false),
        },
        _ => assert!(false),
    }
}