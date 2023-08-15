use crate::environment::*;
use crate::types::*;
use crate::terms::*;
use crate::typechecker::*;
use crate::interpreter::*;

#[allow(dead_code)]
fn test_type_error_helper(path: String) -> (Term, TypeCheckError) { 
    let text = load_program_text(path);
    let ast = parse_program_text(text);
    let error = typecheck(false, TypeEnvironment::new(), &ast);
    match error {
        Ok((_, types)) => panic!("This shouldnt result in a type: {}", types),
        Err(err) => return (ast, err)
    }
}
#[warn(dead_code)]

#[allow(unused_macros)]
macro_rules! test_type_error_result {
    ($path:expr, $expected:expr) => {
        let (ast, result) = test_type_error_helper($path.to_string());
        let reconstructed = reconstruct_source_from_ast(&ast);
        let recon_ast = parse_program_text(reconstructed);
        assert_eq!(result, $expected);
        assert_eq!(ast, recon_ast);
    };
}
#[warn(unused_macros)]

#[allow(unused_macros)]
macro_rules! test_type_error_sample {
    ($name:ident, $path:expr, $expected:expr) => {
        #[test]
        fn $name() {
            test_type_error_result!($path, $expected);
        }
    };
}
#[warn(unused_macros)]

#[allow(dead_code)]
fn test_type_helper(path: String) -> (Term, Type) { 
    let text = load_program_text(path);
    let ast = parse_program_text(text);
    let result = typecheck_program(&ast);
    return (ast, result);
}
#[warn(dead_code)]

#[allow(unused_macros)]
macro_rules! test_type_result {
    ($path:expr, $expected:expr) => {
        let (ast, result) = test_type_helper($path.to_string());
        let reconstructed = reconstruct_source_from_ast(&ast);
        let recon_ast = parse_program_text(reconstructed);
        assert_eq!(result, $expected);
        assert_eq!(ast, recon_ast);
    };
}
#[warn(unused_macros)]

#[allow(unused_macros)]
macro_rules! test_type_sample {
    ($name:ident, $path:expr, $expected:expr) => {
        #[test]
        fn $name() {
            test_type_result!($path, $expected);
        }
    };
}


test_type_sample!(test_type_sample_case, "samples/case.frho", Type::StringType);
test_type_sample!(test_type_typecheck_sample_fib, "typecheck-samples/fib.frho", Type::IntType);
test_type_sample!(test_type_sample_fib, "samples/fib.frho", Type::IntType);
test_type_sample!(test_type_sample_fac, "samples/fac.frho", Type::IntType);
test_type_sample!(test_type_sample_let, "samples/let.frho", Type::IntType);
test_type_sample!(test_type_sample_record_self_assign, "samples/record_self_assign.frho", Type::RecordsType(vec![("test".to_string(), FieldOccurrence::Present(Type::RecordsType(vec![("test".to_string(), FieldOccurrence::Present(Type::RecordsType(vec![])))])))]));
test_type_sample!(test_type_typecheck_sample_records, "typecheck-samples/records.frho", Type::StringType);
test_type_sample!(test_type_sample_records, "samples/records.frho", Type::StringType);
test_type_sample!(test_type_typecheck_sample_recursion, "typecheck-samples/recursion.frho", Type::IntType);
test_type_sample!(test_type_sample_recursion, "samples/recursion.frho", Type::IntType);
test_type_sample!(test_type_sample_return_function_and_call, "samples/return_function_and_call.frho", Type::IntType);
test_type_sample!(test_type_sample_return_function, "samples/return_function.frho", Type::FunctionType(vec![Type::IntType], Box::new(Type::IntType)));
test_type_sample!(test_type_sample_wrap_and_unwrap, "samples/wrap_and_unwrap.frho", Type::IntType);
test_type_sample!(test_type_sample_wrap_and_unwrap_2, "samples/wrap_and_unwrap_2.frho", Type::RecordsType(vec![("pig".to_string(), FieldOccurrence::Present(Type::StringType)), ("swine".to_string(), FieldOccurrence::Absent), ("number".to_string(), FieldOccurrence::Present(Type::IntType))]));
test_type_sample!(test_type_sign_presidence_1, "samples/sign-presidence/1.frho", Type::IntType);
test_type_sample!(test_type_sign_presidence_2, "samples/sign-presidence/2.frho", Type::BoolType);
test_type_sample!(test_type_sign_presidence_3, "samples/sign-presidence/3.frho", Type::BoolType);
test_type_sample!(test_type_typecheck_sample_test_leak, "typecheck-samples/test_leak.frho", Type::IntType);
test_type_sample!(test_type_sample_test_leak, "samples/test_leak.frho", Type::IntType);
test_type_sample!(test_type_typecheck_sample_test_leak_2, "typecheck-samples/test_leak_2.frho", Type::IntType);
test_type_sample!(test_type_sample_test_leak_2, "samples/test_leak_2.frho", Type::IntType);
test_type_sample!(test_type_typecheck_sample_test_leak_3, "typecheck-samples/test_leak_3.frho", Type::IntType);
test_type_sample!(test_type_sample_test_leak_3, "samples/test_leak_3.frho", Type::IntType);
test_type_sample!(test_type_sample_test_leak_4, "samples/test_leak_4.frho", Type::FunctionType(vec![], Box::new(Type::IntType)));
test_type_sample!(test_type_sample_test_leak_5, "samples/test_leak_5.frho", Type::IntType);
test_type_sample!(test_type_sample_test_leak_6, "samples/test_leak_6.frho", Type::IntType);
test_type_sample!(test_type_typecheck_sample_test_leak_7, "typecheck-samples/test_leak_7.frho", Type::IntType);
test_type_sample!(test_type_sample_test_leak_7, "samples/test_leak_7.frho", Type::IntType);
test_type_sample!(test_type_typecheck_sample_test_leak_8_a, "typecheck-samples/test_leak_8_a.frho", Type::IntType);
test_type_sample!(test_type_sample_test_leak_8_a, "samples/test_leak_8_a.frho", Type::IntType);
test_type_sample!(test_type_typecheck_sample_test_leak_8_b, "typecheck-samples/test_leak_8_b.frho", Type::IntType);
test_type_sample!(test_type_sample_test_leak_8_b, "samples/test_leak_8_b.frho", Type::IntType);
test_type_sample!(test_type_typecheck_sample_test_leak_8_c, "typecheck-samples/test_leak_8_c.frho", Type::IntType);
test_type_sample!(test_type_sample_test_leak_8_c, "samples/test_leak_8_c.frho", Type::IntType);
test_type_sample!(test_type_typecheck_sample_test_leak_8_d, "typecheck-samples/test_leak_8_d.frho", Type::IntType);
test_type_sample!(test_type_sample_test_leak_8_d, "samples/test_leak_8_d.frho", Type::IntType);
test_type_sample!(test_type_typecheck_sample_test_leak_8_e, "typecheck-samples/test_leak_8_e.frho", Type::IntType);
test_type_sample!(test_type_sample_test_leak_8_e, "samples/test_leak_8_e.frho", Type::IntType);
test_type_sample!(test_type_multi_recursion, "typecheck-samples/multi_recursion.frho", Type::IntType);
test_type_sample!(test_type_multi_recursion_2, "typecheck-samples/multi_recursion_2.frho", Type::IntType);
test_type_sample!(test_type_sample_variant, "samples/variant.frho", Type::IntType);
test_type_sample!(test_type_sample_count, "samples/count.frho", Type::FunctionType(vec![Type::IntType], Box::new(Type::StringType)));

test_type_error_sample!(test_type_error_difficult_to_determine_dtd_1, "typecheck-samples/difficult-to-determine/dtd_1.frho", TypeCheckError::TypeOfFunctionCannotBeDetermined("fib".to_string()));

test_type_error_sample!(test_type_error_term_checks_if_non_bool_decider, "typecheck-samples/term-checks/if/non-bool-decider.frho", TypeCheckError::WrongType(Type::BoolType, Type::IntType));
test_type_error_sample!(test_type_error_term_checks_if_non_equal_outcomes, "typecheck-samples/term-checks/if/non-equal-outcomes.frho", TypeCheckError::IncompatibleTypes(Type::IntType, Type::StringType));


test_type_error_sample!(test_type_error_term_checks_logicgate_non_possible_negate, "typecheck-samples/term-checks/logicgate/non-possible-negate.frho", TypeCheckError::IncompatibleOperation("Not".to_string(), Type::StringType));
test_type_error_sample!(test_type_error_term_checks_logicgate_non_possible_operation, "typecheck-samples/term-checks/logicgate/non-possible-operation.frho", TypeCheckError::IncompatibleBinOperation(Type::StringType, "Sub".to_string(), Type::StringType));
test_type_sample!(test_type_term_checks_logicgate_possible_operation, "typecheck-samples/term-checks/logicgate/possible-operation.frho", Type::StringType);
test_type_sample!(test_type_term_checks_logicgate_possible_operation_2, "typecheck-samples/term-checks/logicgate/possible-operation-2.frho", Type::FloatType);
test_type_sample!(test_type_term_checks_logicgate_possible_operation_3, "typecheck-samples/term-checks/logicgate/possible-operation-3.frho", Type::FloatType);

test_type_error_sample!(test_type_error_term_checks_function_call_call_non_function, "typecheck-samples/term-checks/function-call/call-non-function.frho", TypeCheckError::WrongType(Type::IntType, Type::FunctionType(vec![], Box::new(Type::NoneType))));
