

pub mod interpreter_evaluation;
pub mod interpreter_environment;
#[cfg(test)]
pub mod interpreter_tests;
pub mod parser_helper;
pub mod interpreter_utils;
#[cfg(test)]
pub mod parser_tests;
#[cfg(test)]
pub mod tests;
pub mod interpreter;


use std::env;

use interpreter::evaluate_program;
use interpreter::load_program_text;
use interpreter::parse_program_text;
use lalrpop_util::lalrpop_mod;

use crate::interpreter::evaluate_program_high_stack_size;

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
    // println!("Hello, world!");
    // 33 constant memory leaks, these are not dependent on count so I guess they are harmless
    // They are dependent on the code used.

    // This shows 33 leaks with leaks on MacOS and 0 leaks with valgrind on Arch Linux
    // interpreter_tests::test_function_call_recursion_abst(200);

    // let res = parser_grammar::CodeParser::new().parse("{}");
    // println!("{:?}", res);

    let args: Vec<String> = env::args().collect();

    let command_str: &str = &(args[1].clone());

    match command_str {
        "interpret" => run_interpret(false, true, args),
        "debug" => run_interpret(true, true, args),
        "debug2" => run_interpret(true, false, args),
        "info" => println!("Frho interpreter by L. Läufer \nUse \"help\" for all commands"),
        "help" => println!("Command list:\ninterpret <path>\ndebug <path>\t\tFor more information while interpreting\ndebug2 <path>\t\tFor more information while interpreting\ninfo\t\t\tInformation about the interpreter\nhelp\t\t\tThis page"),
        _ => println!("Frho interpreter by L. Läufer \nUse \"help\" for all commands"),
    }
}

fn run_interpret(debug: bool, high_stack_size: bool, args: Vec<String>) {
    let text = load_program_text(args[2].clone());
    if debug {println!("Contents: {:?}", text);}
    let ast = parse_program_text(text);
    if debug {println!("AST: {:?}", ast);}
    let result = if high_stack_size {evaluate_program_high_stack_size(ast)} else {evaluate_program(ast)};
    println!("Result: {:?}", result);
}