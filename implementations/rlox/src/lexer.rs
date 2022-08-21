use std::fmt;

use crate::errors;
use crate::scanner;

#[derive(PartialEq)]
pub enum WhitespaceKind {
    Space,
    Tab,
    Return,
    Newline,
}

impl fmt::Debug for WhitespaceKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value = match self {
            WhitespaceKind::Space => "\\s",
            WhitespaceKind::Tab => "\\t",
            WhitespaceKind::Return => "\\r",
            WhitespaceKind::Newline => "\\n",
        };
        write!(f, "{}", value)
    }
}

#[derive(Debug, PartialEq)]
pub enum Token {
    // --- Singles ---
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    QuestionMark,
    Colon,
    Bang,
    Equal,
    Greater,
    Less,
    // --- Pairs ---
    BangEqual,
    EqualEqual,
    GreaterEqual,
    LessEqual,
    // --- Literals ---
    Number(f64),
    // --- Meta ---
    Comment(String),
    Whitespace(WhitespaceKind),
    Eof,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value = match self {
            // --- Singles ---
            Token::LeftParen => String::from("("),
            Token::RightParen => String::from(")"),
            Token::LeftBrace => String::from("{"),
            Token::RightBrace => String::from("}"),
            Token::Comma => String::from(","),
            Token::Dot => String::from("."),
            Token::Minus => String::from("-"),
            Token::Plus => String::from("+"),
            Token::Semicolon => String::from(";"),
            Token::Slash => String::from("/"),
            Token::Star => String::from("*"),
            Token::QuestionMark => String::from("?"),
            Token::Colon => String::from(":"),
            Token::Bang => String::from("!"),
            Token::Equal => String::from("="),
            Token::Greater => String::from(">"),
            Token::Less => String::from("<"),
            // --- Pairs ---
            Token::BangEqual => String::from("!="),
            Token::EqualEqual => String::from("=="),
            Token::GreaterEqual => String::from(">="),
            Token::LessEqual => String::from("<="),
            // --- Literals ---
            Token::Number(number) => format!("number \"{}\"", number),
            // --- Meta ---
            Token::Comment(comment) => format!("comment \"{}\"", comment),
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
        // cursor would have a length of 0. Checking for EOF rather than the pathological case of
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
                // "end" is actually one past the end of a sequence in terms of how we might think
                // of it when looking at it in a text editor.
                self.location.end.column - 1
            )
        };
        write!(f, "({})::{}", self.token, location_string)
    }
}

// -----| Utilities |-----

// This is totally ridiculous. Is there really not a better way of doing this?
// fn grapheme_to_char(symbol: &str) -> char {
//     symbol.to_string().chars().collect::<Vec<char>>()[0]
// }

// This seems a bit wild, but I think it might actually be better than calling `grapheme_to_char()`
// and then `is_ascii_digit()`. Both ways are bad though, need to keep looking for a better way.
fn is_digit(symbol: &str) -> bool {
    match symbol {
        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" => true,
        _ => false,
    }
}

// -----| Lexer |-----

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
                // --- Singles ---
                "(" => Ok(Token::LeftParen),
                ")" => Ok(Token::RightParen),
                "{" => Ok(Token::LeftBrace),
                "}" => Ok(Token::RightBrace),
                "," => Ok(Token::Comma),
                "." => Ok(Token::Dot),
                "-" => Ok(Token::Minus),
                "+" => Ok(Token::Plus),
                ";" => Ok(Token::Semicolon),
                "*" => Ok(Token::Star),
                "?" => Ok(Token::QuestionMark),
                ":" => Ok(Token::Colon),
                // --- Potential Pairs ---
                "!" => {
                    if self.scanner.scan_next_if_match("=") {
                        Ok(Token::BangEqual)
                    } else {
                        Ok(Token::Bang)
                    }
                }
                "=" => {
                    if self.scanner.scan_next_if_match("=") {
                        Ok(Token::EqualEqual)
                    } else {
                        Ok(Token::Equal)
                    }
                }
                "<" => {
                    if self.scanner.scan_next_if_match("=") {
                        Ok(Token::LessEqual)
                    } else {
                        Ok(Token::Less)
                    }
                }
                ">" => {
                    if self.scanner.scan_next_if_match("=") {
                        Ok(Token::GreaterEqual)
                    } else {
                        Ok(Token::Greater)
                    }
                }
                // --- Comment/Division ---
                // It would truly be better to keep these separate and just use `#` for comments.
                "/" => {
                    if self.scanner.scan_next_if_match("/") {
                        let mut content = String::from("//");
                        while let Some(symbol) = self.scanner.peek_next() {
                            if symbol == "\n" {
                                break;
                            }
                            content.push_str(&symbol);
                            self.scanner.scan_next();
                        }
                        Ok(Token::Comment(content))
                    } else {
                        Ok(Token::Slash)
                    }
                }
                // --- Whitespace ---
                " " => Ok(Token::Whitespace(WhitespaceKind::Space)),
                "\r" => Ok(Token::Whitespace(WhitespaceKind::Return)),
                "\t" => Ok(Token::Whitespace(WhitespaceKind::Tab)),
                "\n" => Ok(Token::Whitespace(WhitespaceKind::Newline)),
                digit if is_digit(digit) => self.consume_number(), // better name
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
    fn consume_number(&mut self) -> Result<Token, errors::Error> {
        self.consume_digits();
        if let Some(symbol) = self.scanner.peek_next() {
            if symbol == "." {
                if let Some(symbol) = self.scanner.peek(1) {
                    if is_digit(&symbol) {
                        // Consume the decimal point, then the rest of the number.
                        self.scanner.scan_next();
                        self.consume_digits();
                    } else {
                        // Consume the next two symbols to make the subject/location accurate for
                        // the error message.
                        self.scanner.scan_next();
                        self.scanner.scan_next();
                        return Err(errors::Error {
                            kind: errors::ErrorKind::Scanning,
                            description: errors::ErrorDescription {
                                subject: Some(self.scanner.get_selection()),
                                location: Some(self.scanner.get_cursor()),
                                description: String::from(
                                    "Non-digit encountered after decimal point",
                                ),
                            },
                        });
                    }
                } else {
                    // Consume the next symbol to make the subject/location accurate for the error
                    // message.
                    self.scanner.scan_next();
                    return Err(errors::Error {
                        kind: errors::ErrorKind::Scanning,
                        description: errors::ErrorDescription {
                            subject: Some(self.scanner.get_selection()),
                            location: Some(self.scanner.get_cursor()),
                            description: String::from("Ran out of symbols after decimal point"),
                        },
                    });
                }
            }
        }
        let selection = self.scanner.get_selection();
        let value = selection
            .parse::<f64>()
            .expect(&format!("Internal errors parsing float: {}", selection));
        Ok(Token::Number(value))
    }
    fn consume_digits(&mut self) {
        while let Some(symbol) = self.scanner.peek_next() {
            if is_digit(&symbol) {
                self.scanner.scan_next();
            } else {
                break;
            }
        }
    }
}
