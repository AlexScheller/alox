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
    pub fn expect_and_scan_next(
        &mut self,
        expected: Token,
        error_msg: Option<String>,
    ) -> Result<SourceToken, errors::Error> {
        if let Some(source_token) = self.peek_next() {
            self.scan_next();
            return if enum_variant_equal(&source_token.token, &expected) {
                Ok(source_token)
            } else {
                let description = if let Some(msg) = error_msg {
                    msg
                } else {
                    format!(
                        "Expected '{}' after expression, instead found '{}'",
                        expected, source_token.token
                    )
                };
                Err(errors::Error {
                    kind: errors::ErrorKind::Parsing,
                    description: errors::ErrorDescription {
                        subject: None,
                        location: Some(source_token.location),
                        description,
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
        while let Some(source_token) = self.peek_next() {
            // The book doesn't use an Eof, it requires special handling.
            if source_token.token == lexemes::Token::Eof {
                break;
            }
            self.scan_next();
            if source_token.token == lexemes::Token::Semicolon
                || STATEMENT_BEGINNING_TOKENS.contains(&source_token.token)
            {
                break;
            }
        }
    }
}

// -----| Language Grammer |-----
//
// program  -> declaration* EOF ;

// -----| Declaration Grammar |-----
//
// declaration  -> varDecl | statement ;
// varDecl      -> "var" IDENTIFIER "=" expression ";" ; // I differ from the book here. I disallow uninitialized variables.

// -----| Statement Grammar |-----
//
// statement    -> epxrStmt | ifStmt | whileStmt | block | printStmt ;
// exprStmt     -> expression ";" ;
// ifStmt       -> "if" "(" expression ")" statement ( "else" statement )? ;
// whileStmt    -> "while" "(" expression ")" statement;
// block        -> "{" declaration* "}" ;
// printStmt    -> "print" expression ";" ;

pub struct ExprStmt {
    pub expression: Expr,
}

pub struct IfStmt {
    pub condition: Expr,
    pub then_branch: Box<Stmt>,
    pub else_branch: Option<Box<Stmt>>,
}

pub struct WhileStmt {
    pub condition: Expr,
    pub body: Box<Stmt>,
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
    Block(Vec<Stmt>),
    Expression(ExprStmt),
    If(IfStmt),
    While(WhileStmt),
    Print(PrintStmt),
    Var(VarStmt), // Is this a declaration or a statement?
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value = match self {
            Stmt::Block(stmts) => {
                format!(
                    "{{block {{ {} }} }}",
                    stmts
                        .iter()
                        .map(|stmt| stmt.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Stmt::Expression(stmt) => {
                format!("{{expr {}}}", &stmt.expression)
            }
            Stmt::If(stmt) => {
                if let Some(else_branch) = &stmt.else_branch {
                    format!(
                        "{{if ({}) then {} else {}}}",
                        &stmt.condition, &stmt.then_branch, &else_branch,
                    )
                } else {
                    format!("{{if ({}) then {}}}", &stmt.condition, &stmt.then_branch)
                }
            }
            Stmt::While(stmt) => {
                format!("{{while ({}) {} }}", &stmt.condition, &stmt.body)
            }
            Stmt::Print(stmt) => {
                format!("{{print {}}}", &stmt.expression)
            }
            Stmt::Var(stmt) => {
                format!("{{var {} = {}}}", &stmt.name, &stmt.initializer)
            }
        };
        write!(f, "{}", value)
    }
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

#[derive(Debug)]
pub struct AssignmentExpr {
    pub name: String,
    pub value: Box<Expr>,
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
pub struct LogicalExpr {
    pub left: Box<Expr>,
    pub operator: SourceToken,
    pub right: Box<Expr>,
}

// TODO: Find out if implementing copy is possible. Probably not because of the String.
#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

#[derive(Debug)]
pub enum Expr {
    Assignment(AssignmentExpr),
    Binary(BinaryExpr),
    Ternary(TernaryExpr),
    Grouping(Box<Expr>),
    Unary(UnaryExpr),
    Logical(LogicalExpr),
    Literal(Value),
    Variable(String), // For declarations *and* references?
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
            Expr::Logical(expr) => {
                format!(
                    "({} {} {})",
                    expr.operator.token,
                    format!("{}", &expr.left),
                    format!("{}", &expr.right)
                )
            }
            Expr::Variable(name) => {
                format!("(variable {})", name)
            }
            Expr::Assignment(expr) => {
                format!(
                    "(Assignment {} = {})",
                    expr.name,
                    format!("{}", &expr.value)
                )
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
// expression  -> assignment ;
// assignment  -> IDENTIFIER "=" assignment | ternary; // TODO: Is this correct? even in the book, ternary is handled *before* the assignment logic. Maybe it doesn't matter since it's or?
// ternary     -> logic_or ( "?" logic_or ":" logic_or )* ;
// logic_or    -> logic_and ( "or" logic_and )* ;
// logic_and   -> equality ( "and" equality )* ;
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
        let next = self
            .scanner
            .expect_and_scan_next(IDENTIFIER_EXEMPLAR, None)?;
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
            self.scanner.expect_and_scan_next(
                lexemes::Token::Equal,
                Some(format!(
                    "Expected initializer after variable declaration: 'var {}'",
                    name
                )),
            )?;
            let initializer = self.expression()?;
            self.scanner
                .expect_and_scan_next(lexemes::Token::Semicolon, None)?;
            return Ok(Stmt::Var(VarStmt { name, initializer }));
        };
        panic!("`consume_next_token` has to be broken for this to be reachable");
    }
    // --- Statements ---
    fn statement(&mut self) -> Result<Stmt, errors::Error> {
        // Should this be converted to a `match`?
        if self.scanner.matches_then_scan_next(lexemes::Token::If) {
            return self.if_statement();
        }
        if self.scanner.matches_then_scan_next(lexemes::Token::While) {
            return self.while_statement();
        }
        if self.scanner.matches_then_scan_next(lexemes::Token::Print) {
            return self.print_statement();
        }
        if self
            .scanner
            .matches_then_scan_next(lexemes::Token::LeftBrace)
        {
            return self.block_statement();
        }
        self.expression_statement()
    }
    fn if_statement(&mut self) -> Result<Stmt, errors::Error> {
        self.scanner.expect_and_scan_next(
            lexemes::Token::LeftParen,
            Some(String::from("Expected '(' after 'if'")),
        )?;
        let condition = self.expression()?;
        self.scanner.expect_and_scan_next(
            lexemes::Token::RightParen,
            Some(String::from("Expected ')' after 'if' condition")),
        )?;
        let then_branch = Box::new(self.statement()?);
        let else_branch = if let Some(source_token) = self.scanner.peek_next() {
            if source_token.token == lexemes::Token::Else {
                self.scanner.scan_next();
                Some(Box::new(self.statement()?))
            } else {
                None
            }
        } else {
            None
        };
        Ok(Stmt::If(IfStmt {
            condition,
            then_branch,
            else_branch,
        }))
    }
    fn while_statement(&mut self) -> Result<Stmt, errors::Error> {
        self.scanner.expect_and_scan_next(
            lexemes::Token::LeftParen,
            Some(String::from("Expected '(' after 'while'")),
        )?;
        let condition = self.expression()?;
        self.scanner.expect_and_scan_next(
            lexemes::Token::RightParen,
            Some(String::from("Expected ')' after 'while' condition")),
        )?;
        let body = Box::new(self.statement()?);
        Ok(Stmt::While(WhileStmt { condition, body }))
    }
    fn print_statement(&mut self) -> Result<Stmt, errors::Error> {
        let expression = self.expression()?;
        self.scanner
            .expect_and_scan_next(lexemes::Token::Semicolon, None)?;
        Ok(Stmt::Print(PrintStmt { expression }))
    }
    fn block_statement(&mut self) -> Result<Stmt, errors::Error> {
        let mut statements = Vec::new();
        while let Some(source_token) = self.scanner.peek_next() {
            if source_token.token != lexemes::Token::RightBrace {
                let declaration = self.declaration()?;
                statements.push(declaration);
            } else {
                break;
            }
        }
        self.scanner
            .expect_and_scan_next(lexemes::Token::RightBrace, None)?;
        Ok(Stmt::Block(statements))
    }
    fn expression_statement(&mut self) -> Result<Stmt, errors::Error> {
        let expression = self.expression()?;
        self.scanner
            .expect_and_scan_next(lexemes::Token::Semicolon, None)?;
        Ok(Stmt::Expression(ExprStmt { expression }))
    }
    // --- Expressions ---
    fn expression(&mut self) -> Result<Expr, errors::Error> {
        self.assignment()
    }
    fn assignment(&mut self) -> Result<Expr, errors::Error> {
        let expr = self.ternary()?;
        if let Some(source_token) = self.scanner.peek_next() {
            if source_token.token == lexemes::Token::Equal {
                self.scanner.scan_next();
                // Recursive, because we allow multiple assignment, and it's right-associative.
                // TODO: See if I actually want this.
                let value = self.assignment()?;
                if let Expr::Variable(name) = expr {
                    return Ok(Expr::Assignment(AssignmentExpr {
                        name,
                        value: Box::new(value),
                    }));
                }
                return Err(errors::Error {
                    kind: errors::ErrorKind::Parsing,
                    description: errors::ErrorDescription {
                        subject: None,
                        location: Some(source_token.location),
                        description: format!("Invalid assignment target '{}'", source_token.token),
                    },
                });
            }
        }
        Ok(expr)
    }
    fn ternary(&mut self) -> Result<Expr, errors::Error> {
        let mut expr = self.or()?;
        while let Some(source_token) = self.scanner.peek_next() {
            if source_token.token == TERNARY_TEST_TOKEN {
                self.scanner.scan_next();
                let left_result = self.or()?;
                self.scanner
                    .expect_and_scan_next(TERNARY_BRANCH_TOKEN, None)?;
                let right_result = self.or()?;
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
    fn or(&mut self) -> Result<Expr, errors::Error> {
        let mut expr = self.and()?;
        while let Some(source_token) = self.scanner.peek_next() {
            if source_token.token == lexemes::Token::Or {
                self.scanner.scan_next();
                let right = self.and()?;
                expr = Expr::Logical(LogicalExpr {
                    left: Box::new(expr),
                    operator: source_token,
                    right: Box::new(right),
                });
            } else {
                break;
            }
        }
        Ok(expr)
    }
    fn and(&mut self) -> Result<Expr, errors::Error> {
        let mut expr = self.equality()?;
        while let Some(source_token) = self.scanner.peek_next() {
            if source_token.token == lexemes::Token::And {
                self.scanner.scan_next();
                let right = self.equality()?;
                expr = Expr::Logical(LogicalExpr {
                    left: Box::new(expr),
                    operator: source_token,
                    right: Box::new(right),
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
                    self.scanner.expect_and_scan_next(Token::RightParen, None)?;
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
    fn get_error_log(&mut self) -> &mut errors::ErrorLog {
        &mut self.error_log
    }
}
