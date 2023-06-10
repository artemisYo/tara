// FieldAccess <- "." Ident
use crate::{
    parser::{executable::Executable, Error},
    tokenizer::{Token, Tokenstack},
};

use super::{PERes, PostExecutable};

#[derive(Debug)]
pub struct FieldAccess {
    field: Token,
    object: Box<dyn Executable>,
}
impl PostExecutable for FieldAccess {}
impl Executable for FieldAccess {
    fn as_executable(self: Box<Self>) -> Box<dyn Executable> {
        self
    }
}
pub fn parse(mut input: Tokenstack, exec: Box<dyn Executable>) -> PERes<FieldAccess> {
    if input.pop_if(Token::Accessor).is_none() {
        return Err((exec, Error::Empty));
    }
    let field = match input.pop_if(Token::is_ident) {
        Some(n) => n,
        None => {
            return Err((exec, Error::Empty));
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
