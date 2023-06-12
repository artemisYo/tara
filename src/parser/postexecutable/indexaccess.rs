// IndexAccess <- "[" Executable "]"
use crate::{
    parser::{
        executable::{self, Executable},
        patterns::Pattern,
        Error,
    },
    tokenizer::{Token, Tokenstack},
};

use super::{PERes, PostExecutable};

#[derive(Debug)]
pub struct IndexAccess {
    object: Box<dyn Executable>,
    index: Box<dyn Executable>,
}
impl Pattern for IndexAccess {
    fn as_pattern(self: Box<Self>) -> Box<dyn Pattern> {
        self
    }
}
impl PostExecutable for IndexAccess {}
impl Executable for IndexAccess {
    fn as_executable(self: Box<Self>) -> Box<dyn Executable> {
        self
    }
}
pub fn parse(mut input: Tokenstack, exec: Box<dyn Executable>) -> PERes<IndexAccess> {
    if input.pop_if(Token::OpenBracket).is_none() {
        return Err((exec, Error::Empty));
    }
    let index = match executable::parse(input) {
        Ok((s, a)) => {
            input = s;
            a
        }
        Err(_) => {
            return Err((exec, Error::Empty));
        }
    };
    if input.pop_if(Token::CloseBracket).is_none() {
        return Err((exec, Error::Empty));
    }
    Ok((
        input,
        IndexAccess {
            object: exec,
            index,
        },
    ))
}
