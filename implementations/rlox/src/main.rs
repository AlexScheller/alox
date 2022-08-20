use std::{
    env,
    io::{self, Write},
};

mod errors;
mod lexer;
mod scanner;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() > 1 {
        println!("Usage: garden");
    } else {
        interpret_prompt();
    }
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
        interpret(&line);
    }
}

fn interpret(source: &str) {
    let scanner = scanner::Scanner::new(String::from(source));
    let mut lexer = lexer::Lexer::new(scanner);
    let tokens = lexer.generate_tokens();
    for token in tokens {
        println!("{:?}", token);
    }
}
