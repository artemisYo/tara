use crate::{
    parser::{executable::Executable, Error, PRes},
    tokenizer::{Token, Tokenstack},
};

use super::Expression;

#[derive(Debug)]
pub struct Ident(Token);
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
    let n = input.pop_if(Token::is_ident).ok_or(Error::Empty)?;
    Ok((input, Ident(n)))
}
