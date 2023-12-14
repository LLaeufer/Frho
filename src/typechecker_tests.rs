use crate::environment::*;
use crate::types::*;
use crate::terms::*;
use crate::typechecker::*;
use crate::interpreter::*;

#[allow(dead_code)]
fn test_type_error_helper(path: String) -> (Term, TypeCheckError) { 
    let text = load_program_text(path);
    let ast = parse_program_text(text);
    let error = typecheck(TypeEnvironment::new(), &ast);
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

test_type_sample!(test_type_sample_case, "samples/case.frho", Type::BaseType(BaseType::StringType));
test_type_sample!(test_type_typecheck_sample_fib, "typecheck-samples/fib.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_sample_fib, "samples/fib.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_sample_fac, "samples/fac.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_sample_let, "samples/let.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_sample_record_self_assign, "samples/record_self_assign.frho", Type::RecordsType(vec![("test".to_string(), FieldOccurrence::Present(Type::RecordsType(vec![("test".to_string(), FieldOccurrence::Present(unit()))], RecordAndVariantEnd::Closed)))], RecordAndVariantEnd::Closed));
test_type_sample!(test_type_typecheck_sample_records, "typecheck-samples/records.frho", Type::BaseType(BaseType::StringType));
test_type_sample!(test_type_sample_records, "samples/records.frho", Type::BaseType(BaseType::StringType));
test_type_sample!(test_type_sample_records2, "samples/records2.frho", Type::RecordsType(vec![
    ("fib25".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType))), 
    ("oink".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::StringType))), 
    ("other".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType))),
    ("test".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType)))], RecordAndVariantEnd::Closed));
test_type_sample!(test_type_typecheck_sample_recursion, "typecheck-samples/recursion.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_sample_recursion, "samples/recursion.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_sample_return_function_and_call, "samples/return_function_and_call.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_sample_return_function, "samples/return_function.frho", Type::FunctionType(Box::new(Type::BaseType(BaseType::IntType)), Box::new(Type::BaseType(BaseType::IntType))));
test_type_sample!(test_type_sign_presidence_1, "samples/sign-presidence/1.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_sign_presidence_2, "samples/sign-presidence/2.frho", Type::BaseType(BaseType::BoolType));
test_type_sample!(test_type_sign_presidence_3, "samples/sign-presidence/3.frho", Type::BaseType(BaseType::BoolType));
test_type_sample!(test_type_typecheck_sample_test_leak, "typecheck-samples/test_leak.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_sample_test_leak, "samples/test_leak.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_typecheck_sample_test_leak_2, "typecheck-samples/test_leak_2.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_sample_test_leak_2, "samples/test_leak_2.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_typecheck_sample_test_leak_3, "typecheck-samples/test_leak_3.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_sample_test_leak_3, "samples/test_leak_3.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_sample_test_leak_4, "samples/test_leak_4.frho", Type::FunctionType(Box::new(Type::RecordsType(vec![], RecordAndVariantEnd::Closed)), Box::new(Type::BaseType(BaseType::IntType))));
test_type_sample!(test_type_sample_test_leak_5, "samples/test_leak_5.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_sample_test_leak_6, "samples/test_leak_6.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_typecheck_sample_test_leak_7, "typecheck-samples/test_leak_7.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_sample_test_leak_7, "samples/test_leak_7.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_typecheck_sample_test_leak_8_a, "typecheck-samples/test_leak_8_a.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_sample_test_leak_8_a, "samples/test_leak_8_a.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_typecheck_sample_test_leak_8_b, "typecheck-samples/test_leak_8_b.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_sample_test_leak_8_b, "samples/test_leak_8_b.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_typecheck_sample_test_leak_8_c, "typecheck-samples/test_leak_8_c.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_sample_test_leak_8_c, "samples/test_leak_8_c.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_typecheck_sample_test_leak_8_d, "typecheck-samples/test_leak_8_d.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_sample_test_leak_8_d, "samples/test_leak_8_d.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_typecheck_sample_test_leak_8_e, "typecheck-samples/test_leak_8_e.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_sample_test_leak_8_e, "samples/test_leak_8_e.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_multi_recursion, "typecheck-samples/multi_recursion.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_multi_recursion_2, "typecheck-samples/multi_recursion_2.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_sample_variant, "samples/variant.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_sample_variant_2, "samples/variant2.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_sample_variant_3, "samples/variant3.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_sample_variant_4, "samples/variant4.frho", Type::BaseType(BaseType::StringType));
test_type_sample!(test_type_sample_variant_5, "samples/variant5.frho", Type::BaseType(BaseType::StringType));
test_type_sample!(test_type_sample_count, "samples/count.frho", Type::FunctionType(Box::new(Type::BaseType(BaseType::IntType)), Box::new(Type::BaseType(BaseType::StringType))));
test_type_sample!(test_type_sample_rec_and_save, "samples/rec_and_save.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_sample_dyn_function_for_tests, "samples/dyn_function_for_tests.frho", Type::DynType);

test_type_sample!(test_type_sample_biglambda1, "samples/biglambda1.frho", unit());
test_type_sample!(test_type_sample_biglambda1b, "samples/biglambda1b.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_sample_biglambda2, "samples/biglambda2.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_sample_biglambda3, "samples/biglambda3.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_sample_biglambda4, "samples/biglambda4.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_sample_biglambda5, "samples/biglambda5.frho", Type::BaseType(BaseType::IntType));
test_type_error_sample!(test_type_sample_biglambda6, "samples/biglambda6.frho", TypeCheckError::WrongKind(Kind::Labels(vec!["val".to_string()]), Kind::Labels(vec!["unused".to_string(), "val".to_string()])));
test_type_sample!(test_type_sample_biglambda7, "samples/biglambda7.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_sample_biglambda7b, "samples/biglambda7b.frho", Type::BaseType(BaseType::IntType));

test_type_error_sample!(test_type_error_term_checks_if_non_bool_decider, "typecheck-samples/term-checks/if/non-bool-decider.frho", TypeCheckError::WrongType(Type::BaseType(BaseType::BoolType), Type::BaseType(BaseType::IntType)));
test_type_error_sample!(test_type_error_term_checks_if_non_equal_outcomes, "typecheck-samples/term-checks/if/non-equal-outcomes.frho", TypeCheckError::IncompatibleTypes(Type::BaseType(BaseType::IntType), Type::BaseType(BaseType::StringType)));


test_type_error_sample!(test_type_error_term_checks_logicgate_non_possible_negate, "typecheck-samples/term-checks/logicgate/non-possible-negate.frho", TypeCheckError::IncompatibleOperation("Not".to_string(), Type::BaseType(BaseType::StringType)));
test_type_error_sample!(test_type_error_term_checks_logicgate_non_possible_operation, "typecheck-samples/term-checks/logicgate/non-possible-operation.frho", TypeCheckError::IncompatibleBinOperation(Type::BaseType(BaseType::StringType), "Sub".to_string(), Type::BaseType(BaseType::StringType)));
test_type_sample!(test_type_term_checks_logicgate_possible_operation, "typecheck-samples/term-checks/logicgate/possible-operation.frho", Type::BaseType(BaseType::StringType));
test_type_sample!(test_type_term_checks_logicgate_possible_operation_2, "typecheck-samples/term-checks/logicgate/possible-operation-2.frho", Type::BaseType(BaseType::FloatType));
test_type_sample!(test_type_term_checks_logicgate_possible_operation_3, "typecheck-samples/term-checks/logicgate/possible-operation-3.frho", Type::BaseType(BaseType::FloatType));

test_type_error_sample!(test_type_error_term_checks_function_call_call_non_function, "typecheck-samples/term-checks/function-call/call-non-function.frho", TypeCheckError::WrongType(Type::BaseType(BaseType::IntType), Type::FunctionType(Box::new(unit()), Box::new(unit()))));


// cast tests
test_type_sample!(test_type_casts_1, "samples/casts/1.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_casts_2, "samples/casts/2.frho", Type::DynType);
test_type_sample!(test_type_casts_3, "samples/casts/3.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_casts_4, "samples/casts/4.frho", Type::FunctionType(Box::new(Type::BaseType(BaseType::IntType)), Box::new(Type::DynType)));
test_type_sample!(test_type_casts_5, "samples/casts/5.frho", Type::UniversalType("X".to_string(), Kind::Ty, Box::new(Type::DynType)));
test_type_sample!(test_type_casts_6, "samples/casts/6.frho", Type::DynType);
test_type_sample!(test_type_casts_7, "samples/casts/7.frho", Type::UniversalType("X".to_string(), Kind::Ty, Box::new(Type::BaseType(BaseType::IntType))));
test_type_sample!(test_type_casts_8, "samples/casts/8.frho", Type::RecordsType(vec![("test".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType)))], RecordAndVariantEnd::DynamicEnd));
test_type_sample!(test_type_casts_9, "samples/casts/9.frho", Type::BaseType(BaseType::FloatType));
test_type_sample!(test_type_casts_10, "samples/casts/10.frho", Type::RecordsType(vec![("fib25".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType))),("oink".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::StringType))),("test".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType)))], RecordAndVariantEnd::Closed));
test_type_sample!(test_type_casts_11, "samples/casts/11.frho", Type::BaseType(BaseType::StringType));
test_type_sample!(test_type_casts_12, "samples/casts/12.frho", Type::BaseType(BaseType::StringType));
test_type_sample!(test_type_casts_13, "samples/casts/13.frho", Type::BaseType(BaseType::StringType));
test_type_sample!(test_type_casts_14, "samples/casts/14.frho", Type::RecordsType(vec![("fib5".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType))), ("oink".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::FloatType))), ("test".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType)))], RecordAndVariantEnd::Closed));
test_type_sample!(test_type_casts_15, "samples/casts/15.frho", Type::RecordsType(vec![("fib5".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType))), ("newlabel".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::FloatType))), ("oink".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::StringType))), ("test".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType)))], RecordAndVariantEnd::Closed));
test_type_sample!(test_type_casts_16, "samples/casts/16.frho", Type::RecordsType(vec![("a_bool".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::BoolType))), ("a_number".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::FloatType)))], RecordAndVariantEnd::Closed));
test_type_sample!(test_type_casts_16b, "samples/casts/16b.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_casts_16c, "samples/casts/16c.frho", Type::BaseType(BaseType::FloatType));
test_type_sample!(test_type_casts_16d, "samples/casts/16d.frho", Type::BaseType(BaseType::BoolType));
test_type_sample!(test_type_casts_17, "samples/casts/17.frho", Type::DynType);
test_type_sample!(test_type_casts_18, "samples/casts/18.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_casts_19, "samples/casts/19.frho", Type::BaseType(BaseType::IntType));

// conv tests
test_type_sample!(test_type_conv_1, "samples/conv/1.frho", Type::TypeName("%conv_label".to_string()));
test_type_sample!(test_type_conv_2, "samples/conv/2.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_conv_3, "samples/conv/3.frho", Type::TypeName("%alpha".to_string()));
test_type_sample!(test_type_conv_4, "samples/conv/4.frho", Type::DynType);
test_type_sample!(test_type_conv_5, "samples/conv/5.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_conv_6, "samples/conv/6.frho", Type::FunctionType(Box::new(Type::BaseType(BaseType::IntType)), Box::new(Type::TypeName("%alpha".to_string()))));
test_type_sample!(test_type_conv_7, "samples/conv/7.frho", Type::UniversalType("X".to_string(), Kind::Ty, Box::new(Type::TypeName("%alpha".to_string()))));
test_type_sample!(test_type_conv_8, "samples/conv/8.frho", Type::RecordsType(vec![("fib25".to_string(), FieldOccurrence::Present(Type::TypeName("%conv_label".to_string()))), ("oink".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::StringType))), ("test".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType)))], RecordAndVariantEnd::Closed));
test_type_sample!(test_type_conv_9, "samples/conv/9.frho", Type::TypeName("%conv_label".to_string()));
test_type_sample!(test_type_conv_10, "samples/conv/10.frho", Type::RecordsType(vec![("fib25".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType))), ("oink".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::StringType))), ("test".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType)))], RecordAndVariantEnd::Closed));
test_type_sample!(test_type_conv_11, "samples/conv/11.frho", Type::TypeName("%conv_label".to_string()));
test_type_sample!(test_type_conv_12, "samples/conv/12.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_conv_13, "samples/conv/13.frho", Type::RecordsType(vec![("fib25".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType))), ("oink".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::StringType))), ("test".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType)))], RecordAndVariantEnd::Closed));
test_type_sample!(test_type_conv_14, "samples/conv/14.frho", Type::RecordsType(vec![("fib25".to_string(), FieldOccurrence::Present(Type::TypeName("%conv_label".to_string()))), ("oink".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::StringType))), ("test".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType)))], RecordAndVariantEnd::Closed));
test_type_sample!(test_type_conv_15, "samples/conv/15.frho", Type::RecordsType(vec![("fib25".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType))), ("oink".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::StringType))), ("test".to_string(), FieldOccurrence::Present(Type::BaseType(BaseType::IntType)))], RecordAndVariantEnd::Closed));
test_type_sample!(test_type_conv_16, "samples/conv/16.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_conv_17, "samples/conv/17.frho", Type::BaseType(BaseType::IntType));
test_type_sample!(test_type_conv_17b, "samples/conv/17b.frho", Type::BaseType(BaseType::IntType));