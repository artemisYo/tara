// PlusAndMinus <- ("+" | "-") Executable
use crate::{
    parser::{
        executable::{self, Executable},
        patterns::Pattern,
        postexecutable::{PERes, PostExecutable},
        Error,
    },
    tokenizer::{Token, Tokenstack},
};

use super::Arithmetic;

#[derive(Debug)]
pub struct PlusAndMinus {
    a: Box<dyn Executable>,
    b: Box<dyn Executable>,
    is_plus: bool,
}
impl Pattern for PlusAndMinus {
    fn as_pattern(self: Box<Self>) -> Box<dyn Pattern> {
        self
    }
}
impl Arithmetic for PlusAndMinus {}
impl PostExecutable for PlusAndMinus {}
impl Executable for PlusAndMinus {
    fn as_executable(self: Box<Self>) -> Box<dyn Executable> {
        self
    }
}

const NAME: &'static str = "PlusAndMinus";
pub fn parse(mut input: Tokenstack, exec: Box<dyn Executable>) -> PERes<PlusAndMinus> {
    let is_plus = match input.peek() {
        Token::Plus => {
            input.pop();
            true
        }
        Token::Minus => {
            input.pop();
            false
        }
        _ => {
            return Err((exec, Error::Multiple { possible: &[Token::Plus, Token::Minus], origin: input }));
        }
    };
    let b = match executable::parse(input) {
        Ok((s, b)) => {
            input = s;
            b
        }
        Err(e) => {
            return Err((exec, e.trace(NAME)));
        }
    };
    Ok((
        input,
        PlusAndMinus {
            a: exec,
            b,
            is_plus,
        },
    ))
}
