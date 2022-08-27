use std::{
    env, fs,
    io::{self, Write},
};

use crate::errors::ErrorLoggable;

mod errors;
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
    interpret(contents)
}

fn interpret_prompt() {
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
        interpret(line);
    }
}

fn interpret(source: String) {
    let source = String::from(source);

    let mut lexer = lexer::Lexer::new(source);
    let tokens = lexer.generate_tokens();
    if lexer.error_log().len() > 0 {
        errors::print_error_log(lexer.error_log());
    }

    let mut parser = parser::Parser::new(tokens);
    let expression = parser.expression();
    match expression {
        Ok(expr) => println!("{}", expr),
        Err(err) => {
            println!("{}", err);
            errors::exit_with_code(exitcode::DATAERR);
        }
    }
}
