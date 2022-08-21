use std::{fmt, process};

use crate::scanner;

pub struct ErrorDescription {
    pub subject: Option<String>,
    pub location: Option<scanner::SourceSpan>,
    pub description: String,
}

pub enum ErrorKind {
    Scanning,
    Parsing,
    Runtime,
}

pub struct Error {
    pub kind: ErrorKind,
    pub description: ErrorDescription,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let kind_string = match self.kind {
            ErrorKind::Scanning | ErrorKind::Parsing => String::from("Syntax"),
            ErrorKind::Runtime => String::from("Runtime"),
        };

        let location_string = if let Some(location_value) = self.description.location {
            format!(
                "[line: {}, col: {}] ",
                location_value.begin.line, location_value.begin.column
            )
        } else {
            String::from("")
        };

        let subject_string = if let Some(subject_value) = &self.description.subject {
            format!(": {}", subject_value)
        } else {
            String::from("")
        };

        write!(
            f,
            "{}{} Error ({}){}",
            location_string, kind_string, self.description.description, subject_string
        )
    }
}

pub fn exit_with_code(code: exitcode::ExitCode) {
    process::exit(code);
}
