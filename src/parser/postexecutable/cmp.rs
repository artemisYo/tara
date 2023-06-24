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

const NAME: &'static str = "Comparison";
pub fn parse(mut input: Tokenstack, exec: Box<dyn Executable>) -> PERes<Comparison> {
    let mut op = match input.peek() {
        Token::OpenKeter => Operation::LessThan,
        Token::CloseKeter => Operation::MoreThan,
        Token::Equals => Operation::Equal,
        _ => return Err((exec, Error::Multiple { possible: &[Token::OpenKeter, Token::CloseKeter, Token::Equals], origin: input })),
    };
    input.pop();
    op = match (&op, input.peek()) {
        (Operation::Equal, Token::Equals) => {input.pop(); Operation::Equal},
        (Operation::LessThan, Token::Equals) => {input.pop(); Operation::LessEq},
        (Operation::MoreThan, Token::Equals) => {input.pop(); Operation::MoreEq},
        _ => op,
    };
    let (s, right) = match executable::parse(input) {
        Ok((s, a)) => (s, a),
        Err(e) => return Err((exec, e.trace(NAME))),
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
