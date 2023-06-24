// VariableAssignment <- "=" Executable
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
pub struct Assignment {
    destination: Box<dyn Executable>,
    source: Box<dyn Executable>,
}
impl Pattern for Assignment {
    fn as_pattern(self: Box<Self>) -> Box<dyn Pattern> {
        self
    }
}
impl PostExecutable for Assignment {}
impl Executable for Assignment {
    fn as_executable(self: Box<Self>) -> Box<dyn Executable> {
        self
    }
}

const NAME: &'static str = "Assignment";
pub fn parse(mut input: Tokenstack, exec: Box<dyn Executable>) -> PERes<Assignment> {
    if input.pop_if(Token::Equals).is_none() {
        return Err((exec, Error::Expected { expected: Token::Equals, origin: input }));
    }
    let source = match executable::parse(input) {
        Ok((s, a)) => {
            input = s;
            a
        }
        Err(e) => {
            return Err((exec, e.trace(NAME)));
        }
    };
    Ok((
        input,
        Assignment {
            destination: exec,
            source,
        },
    ))
}
