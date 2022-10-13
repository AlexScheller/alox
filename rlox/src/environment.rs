use std::collections::HashMap;

use crate::{errors, parser::Value};

pub struct Environment {
    values: HashMap<String, Value>,
}

// TODO: See about a better way than cloning values both in an out...
impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
        }
    }
    pub fn define(&mut self, name: &String, value: Value) -> Result<Value, errors::Error> {
        // I differ from the book here. The book allows redefinition
        if self.values.contains_key(name) {
            // Best way to report this error?
            return Err(errors::Error {
                kind: errors::ErrorKind::Runtime,
                description: errors::ErrorDescription {
                    subject: None,
                    // Could eventually actually include the source token rather than just the name
                    // which would allow better error reporting.
                    location: None,
                    description: format!("Attempted redefinition of variable '{}'", name),
                },
            });
        }
        self.values.insert(name.clone(), value.clone());
        Ok(value)
    }
    pub fn assign(&mut self, name: &String, value: Value) -> Result<Value, errors::Error> {
        if self.values.contains_key(name) {
            self.values.insert(name.clone(), value.clone());
        }
        Err(errors::Error {
            kind: errors::ErrorKind::Runtime,
            description: errors::ErrorDescription {
                subject: None,
                // Could eventually actually include the source token rather than just the name
                // which would allow better error reporting.
                location: None,
                description: format!("Undefined variable '{}'", name),
            },
        })
    }
    pub fn get(&mut self, name: &String) -> Result<Value, errors::Error> {
        if let Some(value) = self.values.get(name) {
            return Ok(value.clone());
        }
        Err(errors::Error {
            kind: errors::ErrorKind::Runtime,
            description: errors::ErrorDescription {
                subject: None,
                // Could eventually actually include the source token rather than just the name
                // which would allow better error reporting.
                location: None,
                description: format!("Undefined variable '{}'", name),
            },
        })
    }
}
