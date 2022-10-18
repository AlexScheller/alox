use crate::{
    environment::{self, Environment},
    errors,
    lexemes::Token,
    parser::{AssignmentExpr, BinaryExpr, Expr, Stmt, TernaryExpr, UnaryExpr, Value},
};

pub struct Interpreter {
    environment: environment::Environment,
    error_log: errors::ErrorLog,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            environment: environment::Environment::new(),
            error_log: errors::ErrorLog::new(),
        }
    }
    pub fn interpret(&mut self, statement: Stmt) {
        match statement {
            Stmt::Expression(stmt) => match self.expression(stmt.expression) {
                Ok(_) => {}
                Err(error) => self.error_log.push(error),
            },
            Stmt::If(stmt) => match self.expression(stmt.condition) {
                Ok(value) => {
                    if is_truthy(value) {
                        self.interpret(*stmt.then_branch);
                    } else {
                        if let Some(else_branch) = stmt.else_branch {
                            self.interpret(*else_branch);
                        }
                    }
                }
                Err(error) => self.error_log.push(error),
            },
            Stmt::Print(stmt) => match self.expression(stmt.expression) {
                Ok(value) => {
                    println!("{:?}", value);
                }
                Err(error) => self.error_log.push(error),
            },
            Stmt::Var(stmt) => match self.expression(stmt.initializer) {
                Ok(value) => match self.environment.define(&stmt.name, value) {
                    Ok(_) => (),
                    Err(error) => self.error_log.push(error),
                },
                Err(error) => self.error_log.push(error),
            },
            Stmt::Block(stmts) => {
                self.block(
                    stmts,
                    Environment::new_with_enclosing(self.environment.clone()),
                );
            }
        }
    }
    fn expression(&mut self, expression: Expr) -> Result<Value, errors::Error> {
        match expression {
            Expr::Literal(literal) => Ok(literal),
            Expr::Grouping(group) => self.expression(*group),
            Expr::Unary(unary) => self.unary_expression(unary),
            Expr::Binary(binary) => self.binary_expression(binary),
            Expr::Ternary(ternary) => self.ternary_expression(ternary),
            Expr::Variable(name) => self.environment.get(&name),
            Expr::Assignment(assignment) => self.assignment_expression(assignment),
        }
    }
    fn unary_expression(
        &mut self,
        UnaryExpr { operator, right }: UnaryExpr,
    ) -> Result<Value, errors::Error> {
        let right_literal = self.expression(*right)?;
        match operator.token {
            Token::Minus => {
                if let Value::Number(value) = right_literal {
                    return Ok(Value::Number(-value));
                } else {
                    return Err(construct_runtime_error(format!(
                        "Illegal operand for unary '{}' expression: {:?}",
                        Token::Minus,
                        right_literal
                    )));
                }
            }
            Token::Bang => match right_literal {
                Value::Nil | Value::Boolean(_) => {
                    return Ok(Value::Boolean(!is_truthy(right_literal)));
                }
                _ => {
                    return Err(construct_runtime_error(format!(
                        "Illegal operand for unary '{}' expression: {:?}",
                        Token::Bang,
                        right_literal
                    )));
                }
            },
            // Throughout this file there are panics on illegal operators that should be impossible
            // because the parser should have already caught them. We panic *because* it should be
            // impossible.
            _ => panic!("Illegal operator for unary expression: {}", operator),
        }
    }
    fn binary_expression(
        &mut self,
        BinaryExpr {
            left,
            operator,
            right,
        }: BinaryExpr,
    ) -> Result<Value, errors::Error> {
        let left_literal = self.expression(*left)?;
        let right_literal = self.expression(*right)?;
        let left_value = match left_literal {
            Value::Number(value) => value,
            _ => {
                return Err(construct_runtime_error(format!(
                    "Illegal operand for binary '{}' expression: {:?} {} {:?}",
                    operator.token, left_literal, operator.token, right_literal
                )))
            }
        };
        let right_value = match right_literal {
            Value::Number(value) => value,
            _ => {
                return Err(construct_runtime_error(format!(
                    "Illegal operand for binary '{}' expression: {:?} {} {:?}",
                    operator.token, left_literal, operator.token, right_literal
                )))
            }
        };
        return match operator.token {
            Token::Minus => Ok(Value::Number(left_value - right_value)),
            Token::Slash => Ok(Value::Number(left_value / right_value)),
            Token::Star => Ok(Value::Number(left_value * right_value)),
            Token::Plus => Ok(Value::Number(left_value + right_value)),
            Token::Greater => Ok(Value::Boolean(left_value > right_value)),
            Token::GreaterEqual => Ok(Value::Boolean(left_value >= right_value)),
            Token::Less => Ok(Value::Boolean(left_value < right_value)),
            Token::LessEqual => Ok(Value::Boolean(left_value <= right_value)),
            Token::BangEqual => Ok(Value::Boolean(!is_equal(left_literal, right_literal))),
            Token::EqualEqual => Ok(Value::Boolean(is_equal(left_literal, right_literal))),
            _ => panic!("Illegal operator for binary expression: {}", operator.token),
        };
    }
    fn ternary_expression(
        &mut self,
        TernaryExpr {
            condition,
            left_result,
            right_result,
        }: TernaryExpr,
    ) -> Result<Value, errors::Error> {
        let condition_literal = self.expression(*condition)?;
        // Note, we could check if this is "truthy" instead of an explicit boolean check.
        if let Value::Boolean(condition_value) = condition_literal {
            // This is an important decision. We currently short circuit, but that doesn't mean we
            // must. Perhaps it's valuable to let the other expression evaluate.
            if condition_value {
                return self.expression(*left_result);
            } else {
                return self.expression(*right_result);
            }
        } else {
            return Err(construct_runtime_error(format!(
                "Non boolean type used as condition in ternary: {:?}",
                condition_literal
            )));
        }
    }
    fn assignment_expression(
        &mut self,
        AssignmentExpr { name, value }: AssignmentExpr,
    ) -> Result<Value, errors::Error> {
        let value = self.expression(*value)?;
        self.environment.assign(&name, value.clone())?;
        Ok(value)
    }
    fn block(&mut self, statements: Vec<Stmt>, new_environment: Environment) {
        let previous_environment = self.environment.clone();
        self.environment = new_environment;
        for statement in statements {
            self.interpret(statement);
        }
        self.environment = previous_environment;
    }
}

// -----| Comparison Utilities |-----

trait Boolable {
    fn to_bool_option(&self) -> Option<bool>;
}

impl Boolable for Value {
    fn to_bool_option(&self) -> Option<bool> {
        match self {
            Value::Boolean(value) => Some(*value),
            Value::Nil => Some(false),
            Value::Number(_) => None,
            Value::String(_) => None,
        }
    }
}

fn is_truthy(investigatee: Value) -> bool {
    if let Some(value) = investigatee.to_bool_option() {
        value
    } else {
        false
    }
}

// For now, just relying on PartialEq should be good enough. In the future, this may need to be
// changed, which is why we use this function to wrap the equality check.
fn is_equal(a: Value, b: Value) -> bool {
    a == b
    // Maybe in the future we want to prevent comparisons between types that can never be
    // equivalent. Certainly I have no interest in equality checks succeeding between heterogeneous
    // types of the kind JS allows.
    // if enum_variant_equal(&a, &b) {
    //     return a == b;
    // }
    // panic!("Illegal equality comparison of operands")
}

// -----| Reporting Utilities |-----

// TODO: Use this in the environment class.
fn construct_runtime_error(description: String) -> errors::Error {
    errors::Error {
        kind: errors::ErrorKind::Runtime,
        description: errors::ErrorDescription {
            subject: None,  // TODO?
            location: None, // TODO?
            description,
        },
    }
}

impl errors::ErrorLoggable for Interpreter {
    fn get_error_log(&mut self) -> &mut errors::ErrorLog {
        &mut self.error_log
    }
}
