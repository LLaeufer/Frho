use crate::parser_grammar::*;
use crate::interpreter_environment::*;
use crate::interpreter_utils::*;

use crate::interpreter_evaluation::*;

use std::collections::HashSet;
use std::collections::HashMap;
use std::result;
use std::sync::Arc;


#[test]
fn test_recursion() {
    let parsed = ExprParser::new().parse(r"
    {
        FUN count (a) {
            IF (a == 100) {a;} ELSE {CALL count((a+1),);};
        };
        CALL count(0,);
    }
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