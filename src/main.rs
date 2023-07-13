

pub mod interpreter_evaluation;
pub mod interpreter_environment;
pub mod interpreter_tests;
pub mod parser_helper;
pub mod interpreter_utils;
pub mod parser_tests;
pub mod tests;

// use crate::interpreter_environment::*;

use lalrpop_util::lalrpop_mod;

macro_rules! lalrpop_mod_doc {
    ($vis:vis $name:ident) => {
        lalrpop_util::lalrpop_mod!(
            #[allow(clippy::ptr_arg)]
            #[allow(clippy::vec_box)]
            $vis $name);
    }
}

lalrpop_mod_doc!(pub parser_grammar); // synthesized by LALRPOP

fn main() {
    println!("Hello, world!");
    // 33 constant memory leaks, these are not dependent on count so I guess they are harmless
    // They are dependent on the code used.

    // This shows 33 leaks with leaks on MacOS and 0 leaks with valgrind on Arch Linux
    interpreter_tests::test_function_call_recursion_abst(200);

}

/*
#[test]
fn test_value() {
    assert!(parser_grammar::TermParser::new().parse("7").is_ok());
    let result = parser_grammar::TermParser::new().parse("7").unwrap();
    assert_eq!(result, Value::VInt(7));

}
*/