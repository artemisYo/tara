use crate::{
    parser::{executable::Executable, patterns::Pattern, Error, PRes},
    tokenizer::{Token, Tokenstack},
};

use super::Expression;

#[derive(Debug)]
pub struct Ident(Token);
impl Pattern for Ident {
    fn as_pattern(self: Box<Self>) -> Box<dyn Pattern> {
        self
    }
}
impl Expression for Ident {
    fn as_expression(self: Box<Self>) -> Box<dyn Expression> {
        self
    }
}
impl Executable for Ident {
    fn as_executable(self: Box<Self>) -> Box<dyn Executable> {
        self
    }
}

pub fn parse(mut input: Tokenstack) -> PRes<Ident> {
    let n = input.pop_if(Token::is_ident).ok_or_else(|| Error::Expected { expected: Token::Ident("".to_owned()), origin: input })?;
    Ok((input, Ident(n)))
}
