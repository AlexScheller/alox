use std::{
    env, fs,
    io::{self, Write},
};

use crate::errors::ErrorLoggable;

mod environment;
mod errors;
mod interpreter;
mod lexemes;
mod lexer;
mod parser;
mod source;
mod utilities;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() > 2 {
        println!("Usage: rlox [script]");
        errors::exit_with_code(exitcode::USAGE);
    } else if args.len() == 2 {
        interpret_file(&args[1]);
    } else {
        interpret_prompt();
    }
}

fn interpret_file(file_name: &str) {
    let contents =
        fs::read_to_string(file_name).expect(&format!("Failed to read file: {}", file_name));
    let mut interpreter = interpreter::Interpreter::new();
    interpret(contents, &mut interpreter, true);
}

fn interpret_prompt() {
    let mut interpreter = interpreter::Interpreter::new();
    loop {
        let mut line = String::new();
        print!("> ");
        io::stdout().flush().expect("Failed to flush output");
        io::stdin()
            .read_line(&mut line)
            .expect("Failed to read user input from stdin");
        if line == "\n" {
            break;
        }
        interpret(line, &mut interpreter, false);
    }
}

fn interpret(
    source: String,
    interpreter: &mut interpreter::Interpreter,
    should_exit_on_error: bool,
) {
    let source = String::from(source);

    let mut lexer = lexer::Lexer::new(source);
    let tokens = lexer.generate_tokens();
    if lexer.get_error_log().len() > 0 {
        errors::print_error_log(lexer.get_error_log());
    }

    let mut parser = parser::Parser::new(tokens);
    let statements = parser.parse();
    if parser.get_error_log().len() > 0 {
        errors::print_error_log(parser.get_error_log());
        if should_exit_on_error {
            errors::exit_with_code(exitcode::DATAERR)
        }
    }

    for statement in statements {
        interpreter.interpret(statement);
        if interpreter.get_error_log().len() > 0 {
            errors::print_error_log(interpreter.get_error_log());
            if should_exit_on_error {
                errors::exit_with_code(exitcode::DATAERR)
            }
            interpreter.clear_error_log();
        }
    }
}
