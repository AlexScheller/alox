use std::fmt;

use crate::lexemes::Token;

// -----| Source Location Data |-----

/// A SourceLocation represents the location of a single symbol in the source.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct SourceLocation {
    pub line: usize,   // 1 indexed
    pub column: usize, // 1 indexed
    /// The absolute index into the source, regardless of which line or or column.
    pub index: usize, // 0 indexed
}

impl SourceLocation {
    pub fn new() -> Self {
        SourceLocation {
            line: 1,
            column: 1,
            index: 0,
        }
    }
    fn increment_line(&mut self) {
        self.line += 1;
        self.column = 1;
        self.index += 1;
    }
    fn increment_column(&mut self) {
        self.column += 1;
        self.index += 1;
    }
    pub fn advance(&mut self, symbol: &str) {
        if symbol == "\n" {
            self.increment_line();
        } else {
            self.increment_column();
        }
    }
}

/// SourceSpan represent one to many symbols in linear sequence in source.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct SourceSpan {
    /// Inclusive/Open
    pub begin: SourceLocation,
    /// Exclusive/Closed
    pub end: SourceLocation,
}

impl SourceSpan {
    pub fn new() -> Self {
        SourceSpan {
            begin: SourceLocation::new(),
            end: SourceLocation::new(),
        }
    }
    pub fn length(&self) -> usize {
        return self.end.index - self.begin.index;
    }
    pub fn head(&self) -> usize {
        return self.end.index;
    }
    pub fn extend(&mut self, symbol: &str) {
        self.end.advance(symbol)
    }
    pub fn snap_to_head(&mut self) {
        self.begin = self.end;
    }
}

#[derive(Debug)]
pub struct SourceToken {
    pub token: Token,
    pub location: SourceSpan,
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
