// Deref <- * Executable
use crate::{
    expect,
    parser::{
        executable::{self, Executable},
        PRes,
    },
    tokenizer::{Token, Tokenstack},
};

use super::Expression;

#[derive(Debug)]
pub struct Deref(Box<dyn Executable>);
impl Expression for Deref {
    fn as_expression(self: Box<Self>) -> Box<dyn Expression> {
        self
    }
}
impl Executable for Deref {
    fn as_executable(self: Box<Self>) -> Box<dyn Executable> {
        self
    }
}

pub fn parse(mut input: Tokenstack) -> PRes<Deref> {
    expect!(input, Token::Star);
    let (s, o) = executable::parse(input)?;
    input = s;
    Ok((input, Deref(o)))
}
