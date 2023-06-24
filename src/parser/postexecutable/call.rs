// Call <- "(" ((Ident "=")? Executable ",")* ((Ident "=")? Executable)? ")"
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
    args: Vec<(Option<Token>, Box<dyn Executable>)>,
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

const NAME: &'static str = "Call";
pub fn parse(mut input: Tokenstack, exec: Box<dyn Executable>) -> PERes<Call> {
    if input.pop_if(Token::OpenParen).is_none() {
        return Err((exec, Error::Expected { expected: Token::OpenParen, origin: input }));
    }
    let mut args = vec![];
    // runs any amount of times
    loop {
        if input.next_is(Token::CloseParen) {
            break;
        }
        let n = if input.lookahead(1) == &Token::Equals {
            match input.pop_if(Token::is_ident) {
                Some(n) => {
                    input.pop();
                    Some(n)
                }
                None => {
                    return Err((exec, Error::Expected { expected: Token::Equals, origin: input }));
                }
            }
        } else {
            None
        };
        let (s, a) = match executable::parse(input) {
            Ok((s, a)) => (s, a),
            Err(e) => {
                return Err((exec, e.trace(NAME)));
            }
        };
        args.push((n, a));
        input = s;
        if input.pop_if(Token::Comma).is_none() || input.next_is(Token::CloseParen) {
            break;
        }
    }
    if input.pop_if(Token::CloseParen).is_none() {
        return Err((exec, Error::Expected { expected: Token::CloseParen, origin: input }));
    }
    Ok((input, Call { args, path: exec }))
}
