// Comparison <- ('<=' | '=>' | '<' | '>' | '==') Executable

use crate::{
    parser::{
        executable::{self, Executable},
        patterns::Pattern,
        Error,
    },
    tokenizer::{Token, Tokenstack},
};

use super::PERes;

#[derive(Debug)]
pub enum Operation {
    LessEq,
    MoreEq,
    LessThan,
    MoreThan,
    Equal,
}

#[derive(Debug)]
pub struct Comparison {
    right: Box<dyn Executable>,
    left: Box<dyn Executable>,
    op: Operation,
}
impl Executable for Comparison {
    fn as_executable(self: Box<Self>) -> Box<dyn Executable> {
        self
    }
}
impl Pattern for Comparison {
    fn as_pattern(self: Box<Self>) -> Box<dyn Pattern> {
        self
    }
}

pub fn parse(mut input: Tokenstack, exec: Box<dyn Executable>) -> PERes<Comparison> {
    let mut op = match input.peek() {
        Token::OpenKeter => Operation::LessThan,
        Token::CloseKeter => Operation::MoreThan,
        Token::Equals => Operation::Equal,
        _ => return Err((exec, Error::Empty)),
    };
    input.pop();
    op = match (&op, input.peek()) {
        (Operation::Equal, Token::Equals) => Operation::Equal,
        (Operation::LessThan, Token::Equals) => Operation::LessEq,
        (Operation::MoreThan, Token::Equals) => Operation::MoreEq,
        _ => op,
    };
    let (s, right) = match executable::parse(input) {
        Ok((s, a)) => (s, a),
        Err(_) => return Err((exec, Error::Empty)),
    };
    input = s;
    Ok((
        input,
        Comparison {
            left: exec,
            right,
            op,
        },
    ))
}
