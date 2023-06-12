// OpAssign <- ('+' | '*' | '-' | '/')= Executable

use crate::{
    expect,
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
    Add,
    Sub,
    Mult,
    Div,
}

#[derive(Debug)]
pub struct OpAssign {
    destination: Box<dyn Executable>,
    source: Box<dyn Executable>,
    op: Operation,
}

impl Executable for OpAssign {
    fn as_executable(self: Box<Self>) -> Box<dyn Executable> {
        self
    }
}
impl Pattern for OpAssign {
    fn as_pattern(self: Box<Self>) -> Box<dyn Pattern> {
        self
    }
}

pub fn parse(mut input: Tokenstack, exec: Box<dyn Executable>) -> PERes<OpAssign> {
    let op = match input.peek() {
        Token::Star => Operation::Mult,
        Token::Plus => Operation::Add,
        Token::Minus => Operation::Sub,
        Token::Slash => Operation::Div,
        _ => return Err((exec, Error::Empty)),
    };
    input.pop();
    if input.pop_if(Token::Equals).is_none() {
        return Err((exec, Error::Empty));
    }
    let (s, source) = match executable::parse(input) {
        Ok((s, a)) => (s, a),
        Err(_) => return Err((exec, Error::Empty)),
    };
    input = s;
    Ok((
        input,
        OpAssign {
            destination: exec,
            source,
            op,
        },
    ))
}
