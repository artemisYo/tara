use crate::{
    parser::{executable::Executable, expressions::Expression, patterns::Pattern, Error, PRes},
    tokenizer::{Token, Tokenstack},
};

use super::Literal;

#[derive(Debug)]
pub struct Num(Token);
impl Pattern for Num {
    fn as_pattern(self: Box<Self>) -> Box<dyn Pattern> {
        self
    }
}
impl Literal for Num {}
impl Expression for Num {
    fn as_expression(self: Box<Self>) -> Box<dyn Expression> {
        self
    }
}
impl Executable for Num {
    fn as_executable(self: Box<Self>) -> Box<dyn Executable> {
        self
    }
}

pub fn parse(mut input: Tokenstack) -> PRes<Num> {
    let n = input.pop_if(Token::is_num).ok_or(Error::Empty)?;
    Ok((input, Num(n)))
}
