

pub mod evaluation;
pub mod environment;
#[cfg(test)]
pub mod evaluation_tests;
pub mod parser_helper;
pub mod utils;
#[cfg(test)]
pub mod parser_tests;
#[cfg(test)]
pub mod tests;
pub mod interpreter;
pub mod types;
pub mod terms;
pub mod values;
pub mod logicterms;
pub mod typechecker;
pub mod typechecker_tests;
pub mod parser_preprocessor;


use std::env;

use lalrpop_util::lalrpop_mod;

use crate::{interpreter::*, values::ValueTypes};

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
    let args: Vec<String> = env::args().collect();

    let command: String;
    if args.len()>1 { command = args[1].clone() } else { command = "info".to_string() };

    let command_str: &str = &command;

    match command_str {
        "interpret" => run_typecheck_and_interpret(false, true, args),
        "debug" => run_typecheck_and_interpret(true, true, args),
        "eval-debug" => run_interpret(true, true, args),
        "eval-debug2" => run_interpret(true, false, args),
        "type-debug" => run_typecheck(true, args),
        "info" => println!("Frho interpreter by L. Läufer \nUse \"help\" for all commands"),
        "help" => println!("Command list:\ninterpret <path>\ndebug <path>\t\tFor more information while interpreting\ndebug2 <path>\t\tFor more information while interpreting\ntype-debug <path>\tFor information about the expected type\ninfo\t\t\tInformation about the interpreter\nhelp\t\t\tThis page"),
        _ => println!("Frho interpreter by L. Läufer \nUse \"help\" for all commands"),
    }
}

fn run_typecheck(debug: bool, args: Vec<String>) {
    let text = load_program_text(args[2].clone());
    if debug {println!("Contents: {:?}", text);}
    let ast = parse_program_text(text);
    if debug {println!("AST: {:?}", ast);}
    if debug {println!("Reconstructed: {}", reconstruct_source_from_ast(&ast));}
    let result = typecheck_program(&ast);
    println!("Typecheck Result: {}", result);
}

fn run_interpret(debug: bool, high_stack_size: bool, args: Vec<String>) {
    let text = load_program_text(args[2].clone());
    if debug {println!("Contents: {:?}", text);}
    let ast = parse_program_text(text);
    if debug {println!("AST: {:?}", ast);}
    if debug {println!("Reconstructed: {}", reconstruct_source_from_ast(&ast));}
    let result = if high_stack_size {evaluate_program_high_stack_size(ast)} else {evaluate_program(ast)};
    println!("Result: {}", result);
    println!("Type of Result {}", result.get_type());
    cleanup_after_evaluation(result);
}

fn run_typecheck_and_interpret(debug: bool, high_stack_size: bool, args: Vec<String>) {
    let text = load_program_text(args[2].clone());
    if debug {println!("Contents: {:?}", text);}
    let ast = parse_program_text(text);
    if debug {println!("AST: {:?}", ast);}
    if debug {println!("Reconstructed: {}", reconstruct_source_from_ast(&ast));}
    let typecheck_result = typecheck_program(&ast);
    let result = if high_stack_size {evaluate_program_high_stack_size(ast)} else {evaluate_program(ast)};
    println!("Result: {}", result);
    println!("Typecheck Result: {}", typecheck_result);
    println!("Type of Result {}", result.get_type());
    cleanup_after_evaluation(result);
}