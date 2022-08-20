use std::fmt;

use crate::errors;
use crate::scanner;

#[derive(Debug)]
pub enum WhitespaceKind {
    Space,
    Tab,
    Return,
    Newline,
}

#[derive(Debug)]
pub enum Token {
    LeftParen,
    RightParen,
    Whitespace(WhitespaceKind),
    Eof,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value = match self {
            Token::LeftParen => String::from("("),
            Token::RightParen => String::from(")"),
            Token::Whitespace(whitespace) => format!("whitespace {:?}", whitespace),
            Token::Eof => String::from("Eof"),
        };
        write!(f, "{}", value)
    }
}

#[derive(Debug)]
pub struct SourceToken {
    pub token: Token,
    pub location: scanner::SourceSpan,
}

/// The object through which the source is consumed and transformed into a token sequence
pub struct Lexer {
    scanner: scanner::Scanner,
}

impl Lexer {
    // Constructors
    pub fn new(scanner: scanner::Scanner) -> Self {
        Lexer { scanner }
    }
    // Responsibilities
    pub fn generate_tokens(&mut self) -> Vec<SourceToken> {
        let mut ret = Vec::new();
        while let Some(result) = self.scan_next_token() {
            match result {
                Ok(token) => ret.push(token),
                Err(error) => panic!("{}", error), // TODO: Use ErrorLog or smth
            }
        }
        ret.push(SourceToken {
            token: Token::Eof,
            location: self.scanner.get_cursor(),
        });
        ret
    }
    pub fn scan_next_token(&mut self) -> Option<Result<SourceToken, errors::Error>> {
        if let Some(symbol) = self.consume_next_symbol() {
            let token_result = match symbol.as_ref() {
                "(" => Ok(Token::LeftParen),
                ")" => Ok(Token::RightParen),
                // --- Whitespace ---
                " " => Ok(Token::Whitespace(WhitespaceKind::Space)),
                "\r" => Ok(Token::Whitespace(WhitespaceKind::Return)),
                "\t" => Ok(Token::Whitespace(WhitespaceKind::Tab)),
                "\n" => Ok(Token::Whitespace(WhitespaceKind::Newline)),
                _ => Err(errors::Error {
                    kind: errors::ErrorKind::Scanning,
                    description: errors::ErrorDescription {
                        subject: Some(String::from(symbol)),
                        location: Some(self.scanner.get_cursor()),
                        description: String::from("Unexpected character"),
                    },
                }),
            };
            let ret = match token_result {
                Ok(token) => Some(Ok(SourceToken {
                    token,
                    // This is now prolly going to be wrong with the scanner handling the cursor...
                    location: self.scanner.get_cursor(),
                })),
                Err(error) => Some(Err(error)),
            };
            return ret;
        }
        None
    }
    fn consume_next_symbol(&mut self) -> Option<scanner::Symbol> {
        return self.scanner.scan_curr();
    }
}
