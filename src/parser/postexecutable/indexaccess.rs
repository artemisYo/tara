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

const NAME: &'static str = "IndexAccess";
pub fn parse(mut input: Tokenstack, exec: Box<dyn Executable>) -> PERes<IndexAccess> {
    if input.pop_if(Token::OpenBracket).is_none() {
        return Err((exec, Error::Expected { expected: Token::OpenBracket, origin: input }));
    }
    let index = match executable::parse(input) {
        Ok((s, a)) => {
            input = s;
            a
        }
        Err(e) => {
            return Err((exec, e.trace(NAME)));
        }
    };
    if input.pop_if(Token::CloseBracket).is_none() {
        return Err((exec, Error::Expected { expected: Token::CloseBracket, origin: input }));
    }
    Ok((
        input,
        IndexAccess {
            object: exec,
            index,
        },
    ))
}
