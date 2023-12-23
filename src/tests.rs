use crate::evaluation::*;
use crate::values::*;
use crate::terms::*;
use crate::interpreter::*;
use crate::environment::*;
use crate::types::*;
use crate::utils::*;

use std::collections::HashMap;

pub fn evaluate_program_error(parsed: Term) -> EvaluationError {
    let eval_res = evaluate(Environment::new(), &parsed);
    match eval_res {
        Ok((_, value)) => panic!("This test is supposed to result in an error and not produce {}", value),
        Err(err) => err,
    }
}

pub fn evaluate_program_error_high_stack_size(parsed: Term) -> EvaluationError {
    use std::thread;
    const STACK_SIZE: usize = 8 * 1024 * 1024 * 1024;
    let child = thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(move || evaluate_program_error(parsed))
        .unwrap();

    // Wait for thread to join
    child.join().unwrap()
}

fn test_error_helper(path: String) -> (Term, EvaluationError) { 
    let text = load_program_text(path);
    let ast = parse_program_text(text);
    let result = evaluate_program_error_high_stack_size(ast.clone());
    return (ast, result);
}

macro_rules! test_error_result {
    ($path:expr, $expected:expr) => {
        let (ast, result) = test_error_helper($path.to_string());
        let reconstructed = reconstruct_source_from_ast(&ast);
        let recon_ast = parse_program_text(reconstructed);
        assert_eq!(result, $expected);
        assert_eq!(ast, recon_ast);
    };
}

macro_rules! test_error_sample {
    ($name:ident, $path:expr, $expected:expr) => {
        #[test]
        fn $name() {
            test_error_result!($path, $expected);
        }
    };
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

macro_rules! test_result_check_type {
    ($path:expr, $expected:expr) => {
        let (ast, result) = test_helper($path.to_string());
        let reconstructed = reconstruct_source_from_ast(&ast);
        let recon_ast = parse_program_text(reconstructed);
        assert_eq!(result.get_type(), $expected);
        assert_eq!(ast, recon_ast);
    };
}

macro_rules! test_sample_check_type {
    ($name:ident, $path:expr, $expected:expr) => {
        #[test]
        fn $name() {
            test_result_check_type!($path, $expected);
        }
    };
}

#[test]
fn test_type_env() {
    let mut my_env = TypeEnvironment::new();
    my_env.insert("test".to_string(), Type::DynType);
    assert!(my_env.get(&"test".to_string()).is_some());
    let mut my_clone = my_env.flat_independent_clone();
    my_clone.insert("foo".to_string(), Type::DynType);
    assert!(my_clone.get(&"test".to_string()).is_some());
    assert!(my_clone.get(&"foo".to_string()).is_some());
    assert!(my_env.get(&"foo".to_string()).is_none());
}


test_sample!(test_sample_case, "samples/case.frho", Value::VString("yay".to_string()));
test_sample!(test_sample_fib, "samples/fib.frho", Value::VInt(75025));

test_sample!(test_sample_fac, "samples/fac.frho", Value::VInt(120));
test_sample!(test_sample_fib_anon_rec, "samples/fib_anon_rec.frho", Value::VInt(75025));
test_sample!(test_sample_let, "samples/let.frho", Value::VInt(42));
test_sample!(test_sample_records, "samples/records.frho", Value::VString("piggy".to_string()));
test_sample!(test_sample_records2, "samples/records2.frho", Value::VRecord(value_tuple_to_map(vec![
    ("test".to_string(), Value::VInt(42)),
    ("other".to_string(), Value::VInt(1337)),
    ("oink".to_string(), Value::VString("pig".to_string())), 
    ("fib25".to_string(), Value::VInt(75025))])));
test_sample!(test_sample_recursion, "samples/recursion.frho", Value::VInt(100));
test_sample!(test_sample_recursion100k, "samples/recursion100k.frho", Value::VInt(100_000));
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
test_sample!(test_sample_variant2, "samples/variant2.frho", Value::VInt(0));
test_sample!(test_sample_variant3, "samples/variant3.frho", Value::VInt(-1));
test_sample!(test_sample_variant4, "samples/variant4.frho", Value::VString("We added a friend".to_string()));
test_sample!(test_sample_variant5, "samples/variant5.frho", Value::VString("A new message: Hello".to_string()));
test_sample!(test_sample_rec_and_save, "samples/rec_and_save.frho", Value::VInt(110));
test_sample!(test_sample_dyn_function_for_tests, "samples/dyn_function_for_tests.frho", Value::VFloat(45.0));
test_sample!(test_sample_biglambda1, "samples/biglambda1.frho", unit_value());
test_sample!(test_sample_biglambda1b, "samples/biglambda1b.frho", Value::VInt(42));
test_sample!(test_sample_biglambda2, "samples/biglambda2.frho", Value::VInt(42));
test_sample!(test_sample_biglambda3, "samples/biglambda3.frho", Value::VInt(42));
test_sample!(test_sample_biglambda4, "samples/biglambda4.frho", Value::VInt(42));
test_sample!(test_sample_biglambda5, "samples/biglambda5.frho", Value::VInt(42));
test_sample!(test_sample_biglambda7, "samples/biglambda7.frho", Value::VInt(42));
test_sample!(test_sample_biglambda7b, "samples/biglambda7b.frho", Value::VInt(42));



#[test]
fn test_sample_record_self_assign () {
    let mut inner_map = HashMap::new();
    inner_map.insert("test".to_string(), Value::VRecord(HashMap::new()));
    let mut map = HashMap::new();
    map.insert("test".to_string(), Value::VRecord(inner_map));
    test_result!("samples/record_self_assign.frho", Value::VRecord(map));
}


// casts
test_sample!(test_casts_1, "samples/casts/1.frho", Value::VInt(7));
test_sample!(test_casts_2, "samples/casts/2.frho", Value::VDynamic(Box::new(Value::VInt(7)), GroundType::BaseType(BaseType::IntType), BlameLabel::Present("#blame".to_string())));
test_sample!(test_casts_3, "samples/casts/3.frho", Value::VInt(7));
test_sample_check_type!(test_casts_4, "samples/casts/4.frho", Type::FunctionType(Box::new(Type::BaseType(BaseType::IntType)), Box::new(Type::DynType)));
test_sample_check_type!(test_casts_5, "samples/casts/5.frho", Type::UniversalType("X".to_string(), Kind::Ty, Box::new(Type::DynType)));
test_sample_check_type!(test_casts_6, "samples/casts/6.frho", Type::DynType);
test_sample_check_type!(test_casts_7, "samples/casts/7.frho", Type::UniversalType("X".to_string(), Kind::Ty, Box::new(Type::BaseType(BaseType::IntType))));
test_sample!(test_casts_8, "samples/casts/8.frho", Value::VCast(
    Box::new(Value::VRecord(value_tuple_to_map(vec![("test".to_string(), Value::VInt(42)), ("oink".to_string(), Value::VString("pig".to_string())), ("fib25".to_string(), Value::VInt(75025))]))), 
    Type::RecordsType(vec![("test".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType))), ("fib25".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType))), ("oink".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::StringType)))], RecordAndVariantEnd::Closed).normalize(), 
    BlameLabel::Present("#blame".to_string()), 
    Type::RecordsType(vec![("test".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType)))], RecordAndVariantEnd::DynamicEnd).normalize()));
test_sample!(test_casts_9, "samples/casts/9.frho", Value::VFloat(10.2));
test_sample!(test_casts_12, "samples/casts/12.frho", Value::VString("pig".to_string()));
test_error_sample!(test_casts_13, "samples/casts/13.frho", EvaluationError::Blame(BlameLabel::Present("#blame4".to_string())));
test_sample_check_type!(test_casts_14, "samples/casts/14.frho", Type::RecordsType(vec![("fib5".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType))), ("oink".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::FloatType))), ("test".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType)))], RecordAndVariantEnd::Closed));
test_sample_check_type!(test_casts_15, "samples/casts/15.frho", Type::RecordsType(vec![("fib5".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType))), ("newlabel".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::FloatType))), ("oink".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::StringType))), ("test".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType)))], RecordAndVariantEnd::Closed));
test_sample_check_type!(test_casts_16, "samples/casts/16.frho", Type::RecordsType(vec![("a_bool".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::BoolType))), ("a_number".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::FloatType)))], RecordAndVariantEnd::Closed));
test_sample_check_type!(test_casts_16b, "samples/casts/16b.frho", Type::BaseType(BaseType::IntType));
test_error_sample!(test_casts_16c, "samples/casts/16c.frho", EvaluationError::Blame(BlameLabel::Present("#blame_record_2".to_string())));
test_sample_check_type!(test_casts_16d, "samples/casts/16d.frho", Type::BaseType(BaseType::BoolType));
test_sample!(test_casts_17, "samples/casts/17.frho", Value::VDynamic(Box::new(Value::VInt(42)), GroundType::BaseType(BaseType::IntType), ConvertionOrBlameLabel::Present("#blame".to_string())));
test_sample!(test_casts_18, "samples/casts/18.frho", Value::VInt(42));
test_sample!(test_casts_19, "samples/casts/19.frho", Value::VInt(42));
// test_sample_check_type

// conv

test_sample!(test_conv_1, "samples/conv/1.frho", Value::VSeal(Box::new(Value::VInt(7)), Type::BaseType(BaseType::IntType), "%conv_label".to_string()));
test_sample!(test_conv_2, "samples/conv/2.frho", Value::VInt(7));
test_sample!(test_conv_3, "samples/conv/3.frho", Value::VSeal(Box::new(Value::VInt(7)), Type::BaseType(BaseType::IntType), "%alpha".to_string()));
test_sample!(test_conv_4, "samples/conv/4.frho", Value::VDynamic(Box::new(Value::VInt(7)), GroundType::BaseType(BaseType::IntType), BlameLabel::Present("#blame".to_string())));
test_sample!(test_conv_5, "samples/conv/5.frho", Value::VInt(7));
test_sample_check_type!(test_conv_6, "samples/conv/6.frho", Type::FunctionType(Box::new(Type::BaseType(BaseType::IntType)), Box::new(Type::TypeName("%alpha".to_string()))));
test_sample_check_type!(test_conv_7, "samples/conv/7.frho", Type::UniversalType("X".to_string(), Kind::Ty, Box::new(Type::TypeName("%alpha".to_string()))));
test_sample_check_type!(test_conv_8, "samples/conv/8.frho", Type::RecordsType(vec![("fib25".to_string(), FieldOccurrence::Present(Type::TypeName("%conv_label".to_string()))), ("oink".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::StringType))), ("test".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType)))], RecordAndVariantEnd::Closed));
test_sample_check_type!(test_conv_9, "samples/conv/9.frho", Type::TypeName("%conv_label".to_string()));
test_sample_check_type!(test_conv_10, "samples/conv/10.frho", Type::RecordsType(vec![("fib25".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType))), ("oink".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::StringType))), ("test".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType)))], RecordAndVariantEnd::Closed));
test_sample!(test_conv_11, "samples/conv/11.frho", Value::VSeal(Box::new(Value::VInt(75025)), Type::BaseType(BaseType::IntType), "%conv_label".to_string()));
test_sample!(test_conv_12, "samples/conv/12.frho", Value::VInt(75025));
test_sample!(test_conv_13, "samples/conv/13.frho", Value::VRecord(value_tuple_to_map(vec![("test".to_string(), Value::VInt(42)), ("oink".to_string(), Value::VString("pig".to_string())), ("fib25".to_string(), Value::VInt(1337))])));
test_sample_check_type!(test_conv_14, "samples/conv/14.frho", Type::RecordsType(vec![("fib25".to_string(), FieldOccurrence::Present(Type::TypeName("%conv_label".to_string()))), ("oink".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::StringType))), ("test".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType)))], RecordAndVariantEnd::Closed));
test_sample_check_type!(test_conv_15, "samples/conv/15.frho", Type::RecordsType(vec![("fib25".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType))), ("oink".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::StringType))), ("test".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType)))], RecordAndVariantEnd::Closed));
test_sample!(test_conv_16, "samples/conv/16.frho", Value::VInt(42));
test_sample!(test_conv_17, "samples/conv/17.frho", Value::VInt(42));
test_sample!(test_conv_17b, "samples/conv/17b.frho", Value::VInt(42));
test_sample!(test_conv_18, "samples/conv/18.frho", Value::VInt(42));