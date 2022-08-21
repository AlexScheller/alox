use std::{
    env, fs,
    io::{self, Write},
};

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
    let tokens = lexer.generate_tokens();
    for token in tokens {
        println!("{}", token);
    }
}
