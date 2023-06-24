// FieldAccess <- "." Ident
use crate::{
    parser::{executable::Executable, patterns::Pattern, Error},
    tokenizer::{Token, Tokenstack},
};

use super::{PERes, PostExecutable};

#[derive(Debug)]
pub struct FieldAccess {
    field: Token,
    object: Box<dyn Executable>,
}
impl Pattern for FieldAccess {
    fn as_pattern(self: Box<Self>) -> Box<dyn Pattern> {
        self
    }
}
impl PostExecutable for FieldAccess {}
impl Executable for FieldAccess {
    fn as_executable(self: Box<Self>) -> Box<dyn Executable> {
        self
    }
}

pub fn parse(mut input: Tokenstack, exec: Box<dyn Executable>) -> PERes<FieldAccess> {
    if input.pop_if(Token::Accessor).is_none() {
        return Err((exec, Error::Expected { expected: Token::Accessor, origin: input }));
    }
    let field = match input.pop_if(Token::is_ident) {
        Some(n) => n,
        None => {
            return Err((exec, Error::Expected { expected: Token::Ident("".to_owned()), origin: input }));
        }
    };
    Ok((
        input,
        FieldAccess {
            object: exec,
            field,
        },
    ))
}
