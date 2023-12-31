use crate::parser_grammar::*;
use crate::values::*;
use crate::terms::*;
use crate::logicterms::*;

use std::sync::Arc;

#[test]
fn test_value() {
    assert!(ValueParser::new().parse("7").is_ok());
    let result = ValueParser::new().parse("7").unwrap();
    assert_eq!(result, Value::VInt(7));

    assert!(ValueParser::new().parse("42.3").is_ok());
    let result = ValueParser::new().parse("42.3").unwrap();
    assert_eq!(result, Value::VFloat(42.3));

    assert!(ValueParser::new().parse("\"Test\"").is_ok());
    let result = ValueParser::new().parse("\"Test\"").unwrap();
    assert_eq!(result, Value::VString("Test".to_string()));

    assert!(ValueParser::new().parse("true").is_ok());
    let result = ValueParser::new().parse("true").unwrap();
    assert_eq!(result, Value::VBool(true));

    assert!(ValueParser::new().parse("false").is_ok());
    let result = ValueParser::new().parse("false").unwrap();
    assert_eq!(result, Value::VBool(false));

}


#[test]
fn test_terms() {
    assert!(TermParser::new().parse("7").is_ok());
    let result = TermParser::new().parse("7").unwrap();
    assert_eq!(result, Term::Constant(Value::VInt(7)));

    assert!(TermParser::new().parse("{ test: 42 }").is_ok());
    let result = TermParser::new().parse("{ test: 42 }").unwrap();
    assert_eq!(result, Term::RecordConstruction(vec![("test".to_string(), Box::new(Term::Constant(Value::VInt(42))))]));

    assert!(TermParser::new().parse("{ test: 42, oink: \"pig\" }").is_ok());
    let result = TermParser::new().parse("{ test: 42, oink: \"pig\" }").unwrap();
    assert_eq!(result, Term::RecordConstruction(vec![("test".to_string(), Box::new(Term::Constant(Value::VInt(42)))), ("oink".to_string(), Box::new(Term::Constant(Value::VString("pig".to_string()))))]));

    assert!(TermParser::new().parse("[ test: 42 ]").is_ok());
    let result = TermParser::new().parse("[ test: 42 ]").unwrap();
    assert_eq!(result, Term::VariantConstruction(("test".to_string(), Box::new(Term::Constant(Value::VInt(42))))));
}

#[test]
fn test_unbracketed_term_block() {
    assert!(UnbracketedTermBlockParser::new().parse("7;").is_ok());
    let result = UnbracketedTermBlockParser::new().parse("7;").unwrap();
    assert_eq!(result, Arc::new(vec![Term::Constant(Value::VInt(7))]));
    
}

#[test]
fn test_term_block() {


    assert!(TermBlockParser::new().parse("(7;)").is_ok());
    let result = TermBlockParser::new().parse("(7;)").unwrap();
    assert_eq!(result, Arc::new(vec![Term::Constant(Value::VInt(7))]));
}

#[test]
fn test_math_precedence() {
    assert!(ExprParser::new().parse("7 + 3").is_ok());
    let result = ExprParser::new().parse("7 + 3").unwrap();
    assert_eq!(result, Term::LogicGate(LogicTerm::Add(Box::new(Term::Constant(Value::VInt(7))), Box::new(Term::Constant(Value::VInt(3))))));

    assert!(ExprParser::new().parse("2 * 7 + 3").is_ok());
    let result = ExprParser::new().parse("2 * 7 + 3").unwrap();
    assert_eq!(result, Term::LogicGate(LogicTerm::Add(Box::new(Term::LogicGate(LogicTerm::Mul(Box::new(Term::Constant(Value::VInt(2))), Box::new(Term::Constant(Value::VInt(7)))))), Box::new(Term::Constant(Value::VInt(3))))));

    assert!(ExprParser::new().parse("7 + 3 * 2").is_ok());
    let result = ExprParser::new().parse("7 + 3 * 2").unwrap();
    assert_eq!(result, Term::LogicGate(LogicTerm::Add(Box::new(Term::Constant(Value::VInt(7))), Box::new(Term::LogicGate(LogicTerm::Mul(Box::new(Term::Constant(Value::VInt(3))), Box::new(Term::Constant(Value::VInt(2)))))))));

    assert!(ExprParser::new().parse("7 == 3").is_ok());
    let result = ExprParser::new().parse("7 == 3").unwrap();
    assert_eq!(result, Term::LogicGate(LogicTerm::Eql(Box::new(Term::Constant(Value::VInt(7))), Box::new(Term::Constant(Value::VInt(3))))));
}