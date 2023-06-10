// PlusAndMinus <- ("+" | "-") Executable
use crate::{
    parser::{
        executable::{self, Executable},
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
impl Arithmetic for PlusAndMinus {}
impl PostExecutable for PlusAndMinus {}
impl Executable for PlusAndMinus {
    fn as_executable(self: Box<Self>) -> Box<dyn Executable> {
        self
    }
}

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
            return Err((exec, Error::Empty));
        }
    };
    let b = match executable::parse(input) {
        Ok((s, b)) => {
            input = s;
            b
        }
        Err(_) => {
            return Err((exec, Error::Empty));
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
