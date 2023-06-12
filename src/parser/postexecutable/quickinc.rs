// QuickIncrement <- "++" | "--"
use crate::{
    parser::{executable::Executable, patterns::Pattern, Error},
    tokenizer::{Token, Tokenstack},
};

use super::PERes;

#[derive(Debug)]
pub struct QuickIncrement {
    object: Box<dyn Executable>,
    op: bool,
}

impl Executable for QuickIncrement {
    fn as_executable(self: Box<Self>) -> Box<dyn Executable> {
        self
    }
}
impl Pattern for QuickIncrement {
    fn as_pattern(self: Box<Self>) -> Box<dyn Pattern> {
        self
    }
}

pub fn parse(mut input: Tokenstack, exec: Box<dyn Executable>) -> PERes<QuickIncrement> {
    let op = match input.peek() {
        Token::Plus => true,
        Token::Minus => false,
        _ => return Err((exec, Error::Empty)),
    };
    input.pop();
    if !(op && input.pop_if(Token::Plus).is_some()) {
        return Err((exec, Error::Empty));
    } else if !(!op && input.pop_if(Token::Minus).is_some()) {
        return Err((exec, Error::Empty));
    }
    Ok((input, QuickIncrement { object: exec, op }))
}
