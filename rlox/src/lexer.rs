use unicode_segmentation::UnicodeSegmentation;

use crate::lexemes::{Token, WhitespaceKind};
use crate::{errors, source};

// TODO: put this in a config somewhere?
const USE_EXTENDED_UNICODE: bool = true;

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

// Again, I would really prefer not to do this, but using `grapheme_to_char` seems wrong.
fn is_alpha(symbol: &str) -> bool {
    match symbol {
        "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o"
        | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" => true,
        "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O"
        | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" => true,
        "_" => true, // Hmm, technically not alpha...
        _ => false,
    }
}

fn is_alpha_numeric(symbol: &str) -> bool {
    is_alpha(symbol) || is_digit(symbol)
}

fn match_keyword(symbol: &str) -> Option<Token> {
    match symbol {
        "and" => Some(Token::And),
        "class" => Some(Token::Class),
        "else" => Some(Token::Else),
        "false" => Some(Token::False),
        "for" => Some(Token::For),
        "fun" => Some(Token::Fun),
        "if" => Some(Token::If),
        "nil" => Some(Token::Nil),
        "or" => Some(Token::Or),
        "print" => Some(Token::Print),
        "return" => Some(Token::Return),
        "super" => Some(Token::Super),
        "this" => Some(Token::This),
        "true" => Some(Token::True),
        "var" => Some(Token::Var),
        "while" => Some(Token::While),
        _ => None,
    }
}

// -----| Source Scanner |-----

/// UTF-8 Graphemes
type Symbol = String;

struct Scanner {
    symbols: Vec<Symbol>,
    cursor: source::SourceSpan,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Scanner {
            symbols: source
                .graphemes(USE_EXTENDED_UNICODE)
                .map(|grapheme| String::from(grapheme))
                .collect(),
            cursor: source::SourceSpan::new(),
        }
    }
    pub fn get_cursor(&self) -> source::SourceSpan {
        return self.cursor;
    }
    pub fn get_selection(&self) -> String {
        self.symbols[self.cursor.begin.index..self.cursor.end.index].join("")
    }
    pub fn snap_cursor_to_head(&mut self) {
        self.cursor.snap_to_head();
    }
    // Consumes current symbol (if any) and advances the cursor
    pub fn scan_next(&mut self) -> Option<Symbol> {
        if let Some(curr) = self.symbols.get(self.cursor.head()) {
            self.cursor.extend(curr);
            return Some(String::from(curr));
        }
        None
    }
    // Consumes current symbol if it matches a target, and advances the cursor
    pub fn scan_next_if_match(&mut self, target: &str) -> bool {
        if let Some(curr) = self.symbols.get(self.cursor.head()) {
            if curr == target {
                self.cursor.extend(curr);
                return true;
            }
        }
        false
    }
    pub fn peek_next(&mut self) -> Option<Symbol> {
        self.peek(0)
    }
    pub fn peek(&mut self, steps: usize) -> Option<Symbol> {
        if let Some(curr) = self.symbols.get(self.cursor.head() + steps) {
            return Some(String::from(curr));
        }
        None
    }
}

// -----| Lexer |-----

/// The object through which the source is consumed and transformed into a token sequence
pub struct Lexer {
    scanner: Scanner,
    error_log: errors::ErrorLog,
}

impl Lexer {
    // Constructors
    pub fn new(source: String) -> Self {
        Lexer {
            scanner: Scanner::new(source),
            error_log: errors::ErrorLog::new(),
        }
    }
    // Responsibilities
    pub fn generate_tokens(&mut self) -> Vec<source::SourceToken> {
        let mut ret = Vec::new();
        while let Some(result) = self.scan_next_token() {
            match result {
                Ok(token) => ret.push(token),
                Err(error) => self.error_log.push(error),
            }
        }
        // self.scanner.snap_cursor_to_head();
        ret.push(source::SourceToken {
            token: Token::Eof,
            location: self.scanner.get_cursor(),
        });
        ret
    }
    pub fn scan_next_token(&mut self) -> Option<Result<source::SourceToken, errors::Error>> {
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
                // --- Literals ---
                "\"" => self.consume_string(),
                digit if is_digit(digit) => self.consume_number(),
                identifier if is_alpha(identifier) => self.consume_identifier_or_keyword(),
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
                Ok(token) => Some(Ok(source::SourceToken {
                    token,
                    location: self.scanner.get_cursor(),
                })),
                Err(error) => Some(Err(error)),
            };
            self.scanner.snap_cursor_to_head();
            return ret;
        }
        None
    }
    fn consume_string(&mut self) -> Result<Token, errors::Error> {
        while let Some(symbol) = self.scanner.scan_next() {
            if symbol == "\"" {
                let string_value = self.scanner.get_selection();
                return Ok(Token::String(
                    string_value[1..string_value.len() - 1].to_string(), // Remove double quotes.
                ));
            }
        }
        Err(errors::Error {
            kind: errors::ErrorKind::Scanning,
            description: errors::ErrorDescription {
                subject: Some(self.scanner.get_selection()),
                location: Some(self.scanner.get_cursor()),
                description: String::from("Unterminated String"),
            },
        })
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
    // Will this ever need to be split up?
    fn consume_identifier_or_keyword(&mut self) -> Result<Token, errors::Error> {
        while let Some(symbol) = self.scanner.peek_next() {
            if is_alpha_numeric(&symbol) {
                self.scanner.scan_next();
            } else {
                break;
                // Note that to throw an error here is too eager. Just because we encounter a non
                // alphanumeric character doesn't mean there's an error. The identifier has to
                // terminate *somehow* after all.
                //
                // That being said, right now an incorrectly named identifier will be caught as a
                // parsing error, rather than a lexing error. It would be more helpful to catch it
                // here as a lexing error so that a clearer error message can be dispalyed.
                // To support that, there needs to be some logic built in that determines what
                // characters are acceptable to terminate an identifier or keyword. If the char
                // isn't alphanumeric *and* not one of these acceptable characters, only *then*
                // should an error be thrown.
            }
        }
        let value = self.scanner.get_selection();
        if let Some(keyword) = match_keyword(&value) {
            Ok(keyword)
        } else {
            Ok(Token::Identifier(value))
        }
    }
}

impl errors::ErrorLoggable for Lexer {
    fn get_error_log(&mut self) -> &mut errors::ErrorLog {
        &mut self.error_log
    }
}
