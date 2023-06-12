// Range <- ".." "="? Executable
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
pub struct Range {
    start: Box<dyn Executable>,
    end: Box<dyn Executable>,
    is_inclusive: bool,
}
impl Pattern for Range {
    fn as_pattern(self: Box<Self>) -> Box<dyn Pattern> {
        self
    }
}
impl PostExecutable for Range {}
impl Executable for Range {
    fn as_executable(self: Box<Self>) -> Box<dyn Executable> {
        self
    }
}

pub fn parse(mut input: Tokenstack, exec: Box<dyn Executable>) -> PERes<Range> {
    if input.pop_if(Token::DPeriod).is_none() {
        return Err((exec, Error::Empty));
    }

    let is_inclusive = if input.pop_if(Token::Equals).is_some() {
        true
    } else {
        false
    };
    let (s, end) = match executable::parse(input) {
        Ok((s, a)) => (s, a),
        Err(_) => {
            return Err((exec, Error::Empty));
        }
    };
    input = s;
    Ok((
        input,
        Range {
            start: exec,
            end,
            is_inclusive,
        },
    ))
}
