// PlainBlock <- "{" (Expression ";" | CodeBlock)* (Expression)? "}"
use crate::{
    expect,
    parser::{
        executable::{self, Executable},
        patterns::Pattern,
        postexecutable, PRes,
    },
    tokenizer::{Token, Tokenstack},
};

use super::CodeBlock;

#[derive(Debug)]
pub struct PlainBlock {
    is_implicit: bool,
    body: Vec<Box<dyn Executable>>,
}
impl Pattern for PlainBlock {
    fn as_pattern(self: Box<Self>) -> Box<dyn Pattern> {
        self
    }
}
impl CodeBlock for PlainBlock {}
impl Executable for PlainBlock {
    fn as_executable(self: Box<Self>) -> Box<dyn Executable> {
        self
    }
}
pub fn parse(mut input: Tokenstack) -> PRes<PlainBlock> {
    let mut last_block = false;
    let mut is_implicit = false;
    let mut body: Vec<Box<dyn Executable>> = vec![];
    expect!(input, Token::OpenCurly);
    // runs any amount of times
    loop {
        if input.next_is(Token::CloseCurly) {
            break;
        }
        if let Ok((s, b)) = super::parse(input) {
            let mut i = s;
            let mut exec = b.as_executable();
            loop {
                match postexecutable::parse(i, exec) {
                    Ok((s, a)) => {
                        i = s;
                        exec = a;
                    }
                    Err((a, _)) => {
                        exec = a;
                        break;
                    }
                }
            }
            last_block = true;
            body.push(exec);
            input = i;
        } else if let Ok((s, e)) = executable::parse(input) {
            last_block = false;
            body.push(e);
            input = s;
        }
        if input.pop_if(Token::SemiCol).is_none() && !last_block {
            is_implicit = true;
            break;
        }
        if input.next_is(Token::CloseCurly) {
            break;
        }
    }
    expect!(input, Token::CloseCurly);
    Ok((input, PlainBlock { is_implicit, body }))
}
