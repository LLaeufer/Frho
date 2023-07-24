
use std::fs;

use crate::parser_grammar::*;
use crate::interpreter_environment::*;
use crate::interpreter_evaluation::evaluate;

pub fn load_program_text(path: String) -> String {
    fs::read_to_string(path).expect("File not found!")
}

pub fn parse_program_text(program: String) -> Term {
    let program_str: &str = &program;
    CodeParser::new().parse(program_str).expect("File isn't formated properly!")

}

pub fn evaluate_program(parsed: Term) -> Value {
    let (_, result) = evaluate(Environment::new(), &parsed).expect("Error wile execution");
    result
}

pub fn evaluate_program_high_stack_size(parsed: Term) -> Value {
    use std::thread;
    const STACK_SIZE: usize = 4 * 1024 * 1024 * 1024;
    let child = thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(move || evaluate_program(parsed))
        .unwrap();

    // Wait for thread to join
    child.join().unwrap()
}