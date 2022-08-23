// -----| Parsing |-----

pub struct Parser {
    tokens: Vec<lexer::SourceToken>, // TODO: Move these into their own declaration file
    index: usize,
}
