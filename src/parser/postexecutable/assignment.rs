// VariableAssignment <- "=" Executable
use crate::{
    parser::{
        executable::{self, Executable},
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
impl PostExecutable for Assignment {}
impl Executable for Assignment {
    fn as_executable(self: Box<Self>) -> Box<dyn Executable> {
        self
    }
}
pub fn parse(mut input: Tokenstack, exec: Box<dyn Executable>) -> PERes<Assignment> {
    if input.pop_if(Token::Equals).is_none() {
        return Err((exec, Error::Empty));
    }
    let source = match executable::parse(input) {
        Ok((s, a)) => {
            input = s;
            a
        }
        Err(_) => {
            return Err((exec, Error::Empty));
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
