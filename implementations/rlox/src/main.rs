use std::{
    env, fs,
    io::{self, Write},
};

use crate::errors::ErrorLoggable;

mod errors;
mod lexer;
mod scanner;

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
    let scanner = scanner::Scanner::new(String::from(source));
    let mut lexer = lexer::Lexer::new(scanner);
    let _tokens = lexer.generate_tokens();
    if lexer.error_log().len() > 0 {
        errors::print_error_log(lexer.error_log());
    }
    //     let tokens_without_whitespace = tokens
    //         .into_iter()
    //         .filter(|source_token| match source_token.token {
    //             lexer::Token::Whitespace(any) => false,
    //             _ => true,
    //         })
    //         .collect();
}
