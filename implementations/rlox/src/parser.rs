// -----| Lexeme Scanner |-----

use std::fmt;

use crate::{errors, lexemes, source::SourceToken};

struct Scanner {
    symbols: Vec<SourceToken>,
    index: usize,
}

impl Scanner {
    pub fn new(symbols: Vec<SourceToken>) -> Self {
        Scanner { symbols, index: 0 }
    }
}

// -----| Expression Grammar |-----
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

#[derive(Debug, PartialEq)]
pub enum LiteralKind {
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
    Literal(LiteralKind),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value = match self {
            Expr::Binary(expr) => {
                format!(
                    "({} {} {})",
                    expr.operator,
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
                LiteralKind::Number(number) => number.to_string(),
                LiteralKind::String(string) => string.to_string(),
                LiteralKind::Boolean(boolean) => boolean.to_string(),
                LiteralKind::Nil => String::from("nil"),
            },
            Expr::Unary(expr) => {
                format!("({} {})", expr.operator, format!("{}", &expr.right))
            } // parser::Expr::Variable(expr) => {

              // }
        };
        write!(f, "{}", value)
    }
}

// -----| Token to Expression Sets |-----

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

// -----| Token Exemplars |-----

// TODO: Find out a more rustish way of handling the case where you need to compare the type of enum
// but not the value. Right now I just create "fake" ones as examples.

// const WHITESPACE_EXEMPLAR: lexemes::Token = lexemes::Token::Whitespace(lexemes::WhitespaceKind::Space);

// -----| Parsing |-----

pub struct Parser {
    scanner: Scanner,
    error_log: errors::ErrorLog,
}

// Note that currently the parser assumes that whitespace tokens have been stripped from the
// constructor input.
impl Parser {
    pub fn new(tokens: Vec<SourceToken>) -> Self {
        Parser {
            scanner: Scanner::new(tokens),
            error_log: errors::ErrorLog::new(),
        }
    }
    // TODO: Rename this to `generate_statements` when you being parsing those
    pub fn generate_expressions(&mut self) -> Vec<Expr> {}
}
