use std::fmt;

use crate::errors;
use crate::scanner;

#[derive(Debug, PartialEq)]
pub enum WhitespaceKind {
    Space,
    Tab,
    Return,
    Newline,
}

#[derive(Debug, PartialEq)]
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
            Token::Whitespace(whitespace) => format!("Whitespace {:?}", whitespace),
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

impl fmt::Display for SourceToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // The EOF token is artificially inserted by the lexer when the scanner runs out of tokens.
        // When the scanner runs out of tokens, it doesn't increment its internal cursor, so its
        // cursor would have a length of 0. Checking for EOF rather than the pathololgical case of
        // a zero length cursor is more explicit. A zero length cursor could represent either a
        // scanner that has exhausted the symbol sequence, or a scanner that simply hasn't begun
        // scanning anything with it's current cursor.
        let location_string = if self.location.length() == 1 || self.token == Token::Eof {
            format!(
                "[{}:{}]",
                self.location.begin.line, self.location.begin.column
            )
        } else {
            format!(
                "[{}:{} -> {}:{}]",
                self.location.begin.line,
                self.location.begin.column,
                self.location.end.line,
                self.location.end.column
            )
        };
        write!(f, "\"{}\"::{}", self.token, location_string)
    }
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
        // self.scanner.snap_cursor_to_head();
        ret.push(SourceToken {
            token: Token::Eof,
            location: self.scanner.get_cursor(),
        });
        ret
    }
    pub fn scan_next_token(&mut self) -> Option<Result<SourceToken, errors::Error>> {
        if let Some(symbol) = self.scanner.scan_next() {
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
            self.scanner.snap_cursor_to_head();
            return ret;
        }
        None
    }
}
