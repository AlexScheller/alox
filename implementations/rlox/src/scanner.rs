use unicode_segmentation::UnicodeSegmentation;

const USE_EXTENDED_UNICODE: bool = true;

/// UTF-8 Graphemes
pub type Symbol = String;

pub struct Scanner {
    symbols: Vec<Symbol>,
    cursor: SourceSpan,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Scanner {
            symbols: source
                .graphemes(USE_EXTENDED_UNICODE)
                .map(|grapheme| String::from(grapheme))
                .collect(),
            cursor: SourceSpan::new(),
        }
    }
    pub fn get_cursor(&self) -> SourceSpan {
        return self.cursor;
    }
    // Consumes current symbol (if any) and advances the cursor
    pub fn scan_curr(&mut self) -> Option<Symbol> {
        if let Some(curr) = self.symbols.get(self.cursor.end.index) {
            self.cursor.end.advance(curr);
            return Some(String::from(curr));
        }
        None
    }
}

/// A SourceLocation represents the location of a single symbol in the source.
#[derive(Debug, Clone, Copy)]
pub struct SourceLocation {
    pub line: usize,
    pub column: usize,
    /// The absolute index into the source, regardless of which line or or column.
    pub index: usize,
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
#[derive(Debug, Clone, Copy)]
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
    pub fn close(&mut self) {
        self.begin = self.end;
    }
}
