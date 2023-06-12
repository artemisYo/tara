// Call <- "(" (Executable ",")* Executable? ")"
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
pub struct Call {
    args: Vec<Box<dyn Executable>>,
    path: Box<dyn Executable>,
}
impl Pattern for Call {
    fn as_pattern(self: Box<Self>) -> Box<dyn Pattern> {
        self
    }
}
impl PostExecutable for Call {}
impl Executable for Call {
    fn as_executable(self: Box<Self>) -> Box<dyn Executable> {
        self
    }
}

pub fn parse(mut input: Tokenstack, exec: Box<dyn Executable>) -> PERes<Call> {
    if input.pop_if(Token::OpenParen).is_none() {
        return Err((exec, Error::Empty));
    }
    let mut args = vec![];
    // runs any amount of times
    loop {
        if input.next_is(Token::CloseParen) {
            break;
        }
        let (s, a) = match executable::parse(input) {
            Ok((s, a)) => (s, a),
            Err(_) => {
                return Err((exec, Error::Empty));
            }
        };
        args.push(a);
        input = s;
        if input.pop_if(Token::Comma).is_none() || input.next_is(Token::CloseParen) {
            break;
        }
    }
    if input.pop_if(Token::CloseParen).is_none() {
        return Err((exec, Error::Empty));
    }
    Ok((input, Call { args, path: exec }))
}
