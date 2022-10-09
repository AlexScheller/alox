// -----| Lexeme Scanner |-----

use std::fmt;

use crate::{
    errors,
    lexemes::{self, Token},
    source::SourceToken,
    utilities::enum_variant_equal,
};

struct Scanner {
    symbols: Vec<SourceToken>,
    index: usize,
}

impl Scanner {
    pub fn new(symbols: Vec<SourceToken>) -> Self {
        Scanner { symbols, index: 0 }
    }
    // TODO: I don't want to optimize prematurely, but right now `scan_next` is only used to advance
    // the scanning index, the token returned by `peek_next` is actually what's always used. It's
    // possible we could remove `peek_next` and just always call `scan_next`. Wait until statements
    // are implemented before this occurs.
    pub fn peek_next(&self) -> Option<SourceToken> {
        // The lexer should have inserted an EOF.
        let token = self
            .symbols
            .get(self.index)
            .expect("Consumed all tokens without encountering EOF");
        match token.token {
            lexemes::Token::Eof => None,
            _ => Some(token.clone()),
        }
    }
    // It seems strange to me that scan_next doesn't error when it runs out of characters.
    pub fn scan_next(&mut self) -> Option<SourceToken> {
        if let Some(token) = self.symbols.get(self.index) {
            self.index += 1;
            return Some(token.clone());
        }
        None
    }
    pub fn matches_then_scan_next(&mut self, target: Token) -> bool {
        if let Some(next) = self.peek_next() {
            if next.token == target {
                self.scan_next();
                return true;
            }
        }
        false
    }
    pub fn expect_and_scan_next(&mut self, expected: Token) -> Result<SourceToken, errors::Error> {
        if let Some(source_token) = self.peek_next() {
            self.scan_next();
            return if enum_variant_equal(&source_token.token, &expected) {
                Ok(source_token)
            } else {
                Err(errors::Error {
                    kind: errors::ErrorKind::Parsing,
                    description: errors::ErrorDescription {
                        subject: None,
                        location: Some(source_token.location),
                        description: format!(
                            "Expected '{}' after expression, instead found '{}'",
                            expected, source_token.token
                        ),
                    },
                })
            };
        }
        Err(errors::Error {
            kind: errors::ErrorKind::Parsing,
            description: errors::ErrorDescription {
                subject: None,
                location: None,
                description: format!("Reached end of file while expecting '{}'", expected),
            },
        })
    }
    pub fn peek_previous(&self) -> SourceToken {
        if self.index > 0 {
            return self.symbols.get(self.index - 1).unwrap().clone();
        }
        panic!("Attempted to read previous token while at index 0");
    }
    pub fn synchronize_to_statement_boundary(&mut self) {
        // TODO: Is it an issues that this doesn't somehow error when it reaches EOF without
        // encountering a statment boundary?
        while let Some(source_token) = self.scan_next() {
            if source_token.token == lexemes::Token::Semicolon
                || STATEMENT_BEGINNING_TOKENS.contains(&source_token.token)
            {
                break;
            };
        }
    }
}

// -----| Language Grammer |-----
//
// program  -> declaration* EOF ;

// -----| Declaration Grammar |-----
//
// declaration  -> varDecl | statement ;
// I differ from the book here. I disallow uninitialized variables.
// varDecl      -> "var" IDENTIFIER "=" expression ";" ;

// -----| Statements |-----

// ----- Grammar -----
//
// statement    -> epxrStmt | print Stmt ;
// exprStmt     -> expression ";" ;
// printStmt    -> "print" expression ";" ;

pub struct ExprStmt {
    pub expression: Expr,
}

// TODO: Get rid of this as soon as you have a standard library.
pub struct PrintStmt {
    pub expression: Expr,
}

pub struct VarStmt {
    pub name: String,
    // I differ from the book here by disallowing uninitialized variables. Otherwise this would need
    // to be an Option<Expr>.
    pub initializer: Expr,
}

pub enum Stmt {
    Expression(ExprStmt),
    Print(PrintStmt),
    Var(VarStmt), // Is this a declaration or a statement?
}

const STATEMENT_BEGINNING_TOKENS: &[Token] = &[
    Token::Class,
    Token::For,
    Token::Fun,
    Token::If,
    Token::Print,
    Token::Return,
    Token::Var,
    Token::While,
];

// -----| Expressions |-----

// TODO: Find out if implementing copy is possible. Probably not because of the String.
#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

#[derive(Debug)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: SourceToken,
    pub right: Box<Expr>,
}

// We only have one of these, so the operators are implicit
#[derive(Debug)]
pub struct TernaryExpr {
    pub condition: Box<Expr>,
    pub left_result: Box<Expr>,
    pub right_result: Box<Expr>,
}

#[derive(Debug)]
pub struct UnaryExpr {
    pub operator: SourceToken,
    pub right: Box<Expr>,
}

#[derive(Debug)]
pub enum Expr {
    Binary(BinaryExpr),
    Ternary(TernaryExpr),
    Grouping(Box<Expr>),
    Unary(UnaryExpr),
    Literal(Value),
    Variable(String),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value = match self {
            Expr::Binary(expr) => {
                format!(
                    "({} {} {})",
                    expr.operator.token,
                    format!("{}", &expr.left),
                    format!("{}", &expr.right)
                )
            }
            Expr::Ternary(expr) => {
                format!(
                    "({} ? {} : {})",
                    format!("{}", &expr.condition),
                    format!("{}", &expr.left_result),
                    format!("{}", &expr.right_result),
                )
            }
            Expr::Grouping(expr) => {
                format!("(group {})", format!("{}", &expr))
            }
            Expr::Literal(kind) => match kind {
                Value::Number(number) => number.to_string(),
                Value::String(string) => string.to_string(),
                Value::Boolean(boolean) => boolean.to_string(),
                Value::Nil => String::from("nil"),
            },
            Expr::Unary(expr) => {
                format!("({} {})", expr.operator.token, format!("{}", &expr.right))
            }
            Expr::Variable(name) => {
                format!("(variable {})", name)
            }
        };
        write!(f, "{}", value)
    }
}

// ----- Token to Expression Sets -----

const EQUALITY_TOKENS: &[lexemes::Token] = &[lexemes::Token::BangEqual, lexemes::Token::EqualEqual];

const COMPARISON_TOKENS: &[lexemes::Token] = &[
    lexemes::Token::Greater,
    lexemes::Token::GreaterEqual,
    lexemes::Token::Less,
    lexemes::Token::LessEqual,
];

const TERM_TOKENS: &[lexemes::Token] = &[lexemes::Token::Minus, lexemes::Token::Plus];

const FACTOR_TOKENS: &[lexemes::Token] = &[lexemes::Token::Slash, lexemes::Token::Star];

const UNARY_TOKENS: &[lexemes::Token] = &[lexemes::Token::Bang, lexemes::Token::Minus];

const TERNARY_TEST_TOKEN: lexemes::Token = lexemes::Token::QuestionMark;

const TERNARY_BRANCH_TOKEN: lexemes::Token = lexemes::Token::Colon;

// ----- Grammar -----
//
// In increasing order of precedence
//
// expression  -> ternary ;
//
// expression  -> ternary ;
// ternary     -> equality ( "?" equality ":" equality )* ;
// equality    -> comparison ( ( "!=" | "==" ) comparison )* ;
// comparison  -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term        -> factor ( ( "-" | "+" ) factor )* ;
// factor      -> unary ( ( "/" | "*" ) unary )* ;
// unary       -> ( "!" | "-" ) unary | primary ;
// primary     -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER ;

// -----| Parsing |-----

// This is a recursive descent parser. It starts at the top of the grammar and works it's way down
// while scanning tokens until it reaches a leaf.

pub struct Parser {
    scanner: Scanner,
    error_log: errors::ErrorLog,
}

// Note that currently the parser assumes that whitespace tokens have been stripped from the
// constructor input.
impl Parser {
    pub fn new(tokens: Vec<SourceToken>) -> Self {
        let tokens_without_whitespace = tokens
            .into_iter()
            .filter(|source_token| match &source_token.token {
                Token::Whitespace(_any) => false,
                _ => true,
            })
            .collect();
        Parser {
            scanner: Scanner::new(tokens_without_whitespace),
            error_log: errors::ErrorLog::new(),
        }
    }
    pub fn parse(&mut self) -> Vec<Stmt> {
        // TODO: Maybe find a more accurate name for what this returns. It's kind of weird that we
        // sometimes treat declarations like they are statements, and other times like they are
        // something else entirely.
        let mut statements: Vec<Stmt> = Vec::new();
        while let Some(parse_result) = self.parse_next() {
            match parse_result {
                Ok(statement) => statements.push(statement),
                Err(error) => self.error_log.push(error),
            }
        }
        statements
    }
    fn parse_next(&mut self) -> Option<Result<Stmt, errors::Error>> {
        if let Some(_) = self.scanner.peek_next() {
            return Some(self.declaration());
        }
        None
    }
    // --- Declarations ---
    fn declaration(&mut self) -> Result<Stmt, errors::Error> {
        let res = if self.scanner.matches_then_scan_next(lexemes::Token::Var) {
            self.var_declaration()
        } else {
            self.statement()
        };
        match res {
            Ok(stmt) => Ok(stmt),
            Err(error) => {
                self.scanner.synchronize_to_statement_boundary();
                Err(error)
            }
        }
    }
    fn var_declaration(&mut self) -> Result<Stmt, errors::Error> {
        // TODO: Find a way to either make this a constant, or to pass just the kind of an enum, and
        // not an instance of it.
        let IDENTIFIER_EXEMPLAR = lexemes::Token::Identifier(String::from("example"));
        let next = self.scanner.expect_and_scan_next(IDENTIFIER_EXEMPLAR)?;
        // Quite the if-let/deconstruction here...
        if let SourceToken {
            token: lexemes::Token::Identifier(name),
            ..
        } = next
        {
            // This commented out version allows uninitialized variables.
            // let mut initializer = None;
            // let source_token = self.advance_token_index()?;
            // if self.match_then_consume(source_token.token, scanner::Token::Equal) {
            //     initializer = Some(self.expression()?);
            // }
            // self.consume_next_token(scanner::Token::Semicolon)?;
            // return Ok(Stmt::Var(VarStmt { name, initializer }));

            // I differ from the book here, by disallowing uninitialized variables.
            self.scanner.expect_and_scan_next(lexemes::Token::Equal)?;
            let initializer = self.expression()?;
            self.scanner
                .expect_and_scan_next(lexemes::Token::Semicolon)?;
            return Ok(Stmt::Var(VarStmt { name, initializer }));
        };
        panic!("`consume_next_token` has to be broken for this to be reachable");
    }
    // --- Statements ---
    fn statement(&mut self) -> Result<Stmt, errors::Error> {
        if self.scanner.matches_then_scan_next(lexemes::Token::Print) {
            return self.print_statement();
        }
        self.expression_statement()
    }
    fn print_statement(&mut self) -> Result<Stmt, errors::Error> {
        let expression = self.expression()?;
        self.scanner
            .expect_and_scan_next(lexemes::Token::Semicolon)?;
        Ok(Stmt::Print(PrintStmt { expression }))
    }
    fn expression_statement(&mut self) -> Result<Stmt, errors::Error> {
        let expression = self.expression()?;
        self.scanner
            .expect_and_scan_next(lexemes::Token::Semicolon)?;
        Ok(Stmt::Expression(ExprStmt { expression }))
    }
    // --- Expressions ---
    fn expression(&mut self) -> Result<Expr, errors::Error> {
        self.ternary()
    }
    fn ternary(&mut self) -> Result<Expr, errors::Error> {
        let mut expr = self.equality()?;
        while let Some(source_token) = self.scanner.peek_next() {
            if source_token.token == TERNARY_TEST_TOKEN {
                self.scanner.scan_next();
                let left_result = self.equality()?;
                self.scanner.expect_and_scan_next(TERNARY_BRANCH_TOKEN)?;
                let right_result = self.equality()?;
                expr = Expr::Ternary(TernaryExpr {
                    condition: Box::new(expr),
                    left_result: Box::new(left_result),
                    right_result: Box::new(right_result),
                });
            } else {
                break;
            }
        }
        Ok(expr)
    }
    // Note that the binary expressions are all the same code, just with different tokens to check
    // against, and functions to call. This redundancy *could* be reduced.
    fn equality(&mut self) -> Result<Expr, errors::Error> {
        let mut expr = self.comparison()?;
        while let Some(source_token) = self.scanner.peek_next() {
            if EQUALITY_TOKENS.contains(&source_token.token) {
                self.scanner.scan_next();
                let right = self.comparison()?;
                expr = Expr::Binary(BinaryExpr {
                    left: Box::new(expr),
                    operator: source_token,
                    right: Box::new(right),
                })
            } else {
                break;
            }
        }
        Ok(expr)
    }
    fn comparison(&mut self) -> Result<Expr, errors::Error> {
        let mut expr = self.term()?;
        while let Some(source_token) = self.scanner.peek_next() {
            if COMPARISON_TOKENS.contains(&source_token.token) {
                self.scanner.scan_next();
                let right = self.term()?;
                expr = Expr::Binary(BinaryExpr {
                    left: Box::new(expr),
                    operator: source_token,
                    right: Box::new(right),
                })
            } else {
                break;
            }
        }
        Ok(expr)
    }
    fn term(&mut self) -> Result<Expr, errors::Error> {
        let mut expr = self.factor()?;
        while let Some(source_token) = self.scanner.peek_next() {
            if TERM_TOKENS.contains(&source_token.token) {
                self.scanner.scan_next();
                let right = self.factor()?;
                expr = Expr::Binary(BinaryExpr {
                    left: Box::new(expr),
                    operator: source_token,
                    right: Box::new(right),
                })
            } else {
                break;
            }
        }
        Ok(expr)
    }
    fn factor(&mut self) -> Result<Expr, errors::Error> {
        let mut expr = self.unary()?;
        while let Some(source_token) = self.scanner.peek_next() {
            if FACTOR_TOKENS.contains(&source_token.token) {
                self.scanner.scan_next();
                let right = self.unary()?;
                expr = Expr::Binary(BinaryExpr {
                    left: Box::new(expr),
                    operator: source_token,
                    right: Box::new(right),
                })
            } else {
                break;
            }
        }
        Ok(expr)
    }
    fn unary(&mut self) -> Result<Expr, errors::Error> {
        if let Some(source_token) = self.scanner.peek_next() {
            if UNARY_TOKENS.contains(&source_token.token) {
                self.scanner.scan_next();
                let right = self.unary()?;
                return Ok(Expr::Unary(UnaryExpr {
                    operator: source_token,
                    right: Box::new(right),
                }));
            }
        }
        self.primary()
    }
    fn primary(&mut self) -> Result<Expr, errors::Error> {
        if let Some(source_token) = self.scanner.peek_next() {
            self.scanner.scan_next();
            match source_token.token {
                Token::False => Ok(Expr::Literal(Value::Boolean(false))),
                Token::True => Ok(Expr::Literal(Value::Boolean(true))),
                Token::Nil => Ok(Expr::Literal(Value::Nil)),
                Token::Number(value) => Ok(Expr::Literal(Value::Number(value))),
                Token::String(value) => Ok(Expr::Literal(Value::String(value))),
                Token::LeftParen => {
                    let expr = self.expression()?;
                    self.scanner.expect_and_scan_next(Token::RightParen)?;
                    Ok(Expr::Grouping(Box::new(expr)))
                }
                Token::Identifier(name) => Ok(Expr::Variable(name)),
                _ => Err(errors::Error {
                    kind: errors::ErrorKind::Parsing,
                    description: errors::ErrorDescription {
                        subject: None,
                        location: Some(source_token.location),
                        description: format!(
                            "Expected value or expression, found '{}'",
                            source_token.token
                        ),
                    },
                }),
            }
        } else {
            Err(errors::Error {
                kind: errors::ErrorKind::Parsing,
                description: errors::ErrorDescription {
                    subject: None,
                    location: Some(self.scanner.peek_previous().location),
                    description: String::from("Ran out of tokens while satisfying expression rule"),
                },
            })
        }
    }
}

impl errors::ErrorLoggable for Parser {
    fn error_log(&self) -> &errors::ErrorLog {
        &self.error_log
    }
}
