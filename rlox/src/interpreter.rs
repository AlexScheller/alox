use crate::{
    errors,
    lexemes::Token,
    parser::{BinaryExpr, Expr, LiteralKind, Stmt, TernaryExpr, UnaryExpr},
};

pub struct Interpreter {
    error_log: errors::ErrorLog,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            error_log: errors::ErrorLog::new(),
        }
    }
    pub fn interpret(&self, statement: Stmt) {
        match statement {
            Stmt::Expression(stmt) => match self.expression(stmt.expression) {
                Ok(_) => {}
                Err(error) => self.error_log.push(error),
            },
            Stmt::Print(stmt) => match self.expression(stmt.expression) {
                Ok(value) => {
                    println!("{:?}", value);
                }
                Err(error) => self.error_log.push(error),
            },
        }
    }
    fn expression(&self, expression: Expr) -> Result<LiteralKind, errors::Error> {
        match expression {
            Expr::Literal(literal) => Ok(literal),
            Expr::Grouping(group) => self.expression(*group),
            Expr::Unary(unary) => self.unary_expression(unary),
            Expr::Binary(binary) => self.binary_expression(binary),
            Expr::Ternary(ternary) => self.ternary_expression(ternary),
        }
    }
    fn unary_expression(
        &self,
        UnaryExpr { operator, right }: UnaryExpr,
    ) -> Result<LiteralKind, errors::Error> {
        let right_literal = self.expression(*right)?;
        match operator.token {
            Token::Minus => {
                if let LiteralKind::Number(value) = right_literal {
                    return Ok(LiteralKind::Number(-value));
                } else {
                    return Err(construct_runtime_error(format!(
                        "Illegal operand for unary '{}' expression: {:?}",
                        Token::Minus,
                        right_literal
                    )));
                }
            }
            Token::Bang => match right_literal {
                LiteralKind::Nil | LiteralKind::Boolean(_) => {
                    return Ok(LiteralKind::Boolean(!is_truthy(right_literal)));
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
        &self,
        BinaryExpr {
            left,
            operator,
            right,
        }: BinaryExpr,
    ) -> Result<LiteralKind, errors::Error> {
        let left_literal = self.expression(*left)?;
        let right_literal = self.expression(*right)?;
        let left_value = match left_literal {
            LiteralKind::Number(value) => value,
            _ => {
                return Err(construct_runtime_error(format!(
                    "Illegal operand for binary '{}' expression: {:?} {} {:?}",
                    operator.token, left_literal, operator.token, right_literal
                )))
            }
        };
        let right_value = match right_literal {
            LiteralKind::Number(value) => value,
            _ => {
                return Err(construct_runtime_error(format!(
                    "Illegal operand for binary '{}' expression: {:?} {} {:?}",
                    operator.token, left_literal, operator.token, right_literal
                )))
            }
        };
        return match operator.token {
            Token::Minus => Ok(LiteralKind::Number(left_value - right_value)),
            Token::Slash => Ok(LiteralKind::Number(left_value / right_value)),
            Token::Star => Ok(LiteralKind::Number(left_value * right_value)),
            Token::Plus => Ok(LiteralKind::Number(left_value + right_value)),
            Token::Greater => Ok(LiteralKind::Boolean(left_value > right_value)),
            Token::GreaterEqual => Ok(LiteralKind::Boolean(left_value >= right_value)),
            Token::Less => Ok(LiteralKind::Boolean(left_value < right_value)),
            Token::LessEqual => Ok(LiteralKind::Boolean(left_value <= right_value)),
            Token::BangEqual => Ok(LiteralKind::Boolean(!is_equal(left_literal, right_literal))),
            Token::EqualEqual => Ok(LiteralKind::Boolean(is_equal(left_literal, right_literal))),
            _ => panic!("Illegal operator for binary expression: {}", operator.token),
        };
    }
    fn ternary_expression(
        &self,
        TernaryExpr {
            condition,
            left_result,
            right_result,
        }: TernaryExpr,
    ) -> Result<LiteralKind, errors::Error> {
        let condition_literal = self.expression(*condition)?;
        // Note, we could check if this is "truthy" instead of an explicit boolean check.
        if let LiteralKind::Boolean(condition_value) = condition_literal {
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
}

// -----| Comparison Utilities |-----

trait Boolable {
    fn to_bool_option(&self) -> Option<bool>;
}

impl Boolable for LiteralKind {
    fn to_bool_option(&self) -> Option<bool> {
        match self {
            LiteralKind::Boolean(value) => Some(*value),
            LiteralKind::Nil => Some(false),
            LiteralKind::Number(_) => None,
            LiteralKind::String(_) => None,
        }
    }
}

fn is_truthy(investigatee: LiteralKind) -> bool {
    if let Some(value) = investigatee.to_bool_option() {
        value
    } else {
        false
    }
}

// For now, just relying on PartialEq should be good enough. In the future, this may need to be
// changed, which is why we use this function to wrap the equality check.
fn is_equal(a: LiteralKind, b: LiteralKind) -> bool {
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
