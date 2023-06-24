// ForStatement <- "for" Ident "in" Executable CodeBlock
use crate::{
    parser::{
        codeblocks,
        executable::{self, Executable},
        patterns::Pattern,
        Error, PRes, Traceable,
    },
    tokenizer::{Token, Tokenstack}, expect,
};

use super::CodeBlock;

#[derive(Debug)]
pub struct ForStatement {
    name: Token,
    source: Box<dyn Executable>,
    body: Box<dyn CodeBlock>,
}
impl Pattern for ForStatement {
    fn as_pattern(self: Box<Self>) -> Box<dyn Pattern> {
        self
    }
}
impl CodeBlock for ForStatement {}
impl Executable for ForStatement {
    fn as_executable(self: Box<Self>) -> Box<dyn Executable> {
        self
    }
}
const NAME: &'static str = "ForStatement";
pub fn parse(mut input: Tokenstack) -> PRes<ForStatement> {
    //input.pop_if(Token::ForKey).ok_or_else(|| Error::Expected { expected: Token::ForKey, found: input.pop(), origin: input })?;
    expect!(input, Token::ForKey);
    let name = input.pop_if(Token::is_ident).ok_or(Error::Empty)?;
    //input.pop_if(Token::InKey).ok_or_else(|| Error::Expected { expected: Token::InKey, found: input.pop(), origin: input })?;
    expect!(input, Token::InKey);
    let (s, source) = executable::parse(input).trace(NAME)?;
    input = s;
    let (s, body) = codeblocks::parse(input).trace(NAME)?;
    input = s;
    Ok((input, ForStatement { name, source, body }))
}
