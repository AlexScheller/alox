use std::fmt;

#[derive(PartialEq, Clone)]
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

pub type Identifier = String;

#[derive(Debug, PartialEq, Clone)]
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
    Identifier(Identifier), // Note if this ever changes then other representations of identifiers will need to also.
    String(String),
    Number(f64),
    // --- Keywords ---
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
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
            Token::Identifier(identifier) => format!("identifier \"{}\"", identifier),
            Token::String(string) => format!("string \"{}\"", string),
            Token::Number(number) => format!("number \"{}\"", number),
            // --- Keywords ---
            Token::And => String::from("and"),
            Token::Class => String::from("class"),
            Token::Else => String::from("else"),
            Token::False => String::from("false"),
            Token::Fun => String::from("fun"),
            Token::For => String::from("for"),
            Token::If => String::from("if"),
            Token::Nil => String::from("nil"),
            Token::Or => String::from("or"),
            Token::Print => String::from("print"),
            Token::Return => String::from("return"),
            Token::Super => String::from("super"),
            Token::This => String::from("this"),
            Token::True => String::from("true"),
            Token::Var => String::from("let"),
            Token::While => String::from("while"),
            // --- Meta ---
            Token::Comment(comment) => format!("comment \"{}\"", comment),
            Token::Whitespace(whitespace) => format!("Whitespace {:?}", whitespace),
            Token::Eof => String::from("Eof"),
        };
        write!(f, "{}", value)
    }
}
