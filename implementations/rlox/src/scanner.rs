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
    pub fn match_scan_next(&mut self, target: &str) -> bool {
        if let Some(curr) = self.symbols.get(self.cursor.head()) {
            if curr == target {
                self.cursor.extend(curr);
                return true;
            }
        }
        false
    }
    pub fn peek_next(&mut self) -> Option<Symbol> {
        if let Some(curr) = self.symbols.get(self.cursor.head()) {
            return Some(String::from(curr));
        }
        None
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
