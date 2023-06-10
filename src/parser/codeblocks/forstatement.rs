// ForStatement <- "for" Ident "in" Executable CodeBlock
use crate::{
    expect,
    parser::{
        codeblocks,
        executable::{self, Executable},
        Error, PRes,
    },
    tokenizer::{Token, Tokenstack},
};

use super::CodeBlock;

#[derive(Debug)]
pub struct ForStatement {
    name: Token,
    source: Box<dyn Executable>,
    body: Box<dyn CodeBlock>,
}
impl CodeBlock for ForStatement {}
impl Executable for ForStatement {
    fn as_executable(self: Box<Self>) -> Box<dyn Executable> {
        self
    }
}
pub fn parse(mut input: Tokenstack) -> PRes<ForStatement> {
    expect!(input, Token::ForKey);
    let name = input.pop_if(Token::is_ident).ok_or(Error::Empty)?;
    expect!(input, Token::InKey);
    let (s, source) = executable::parse(input)?;
    input = s;
    let (s, body) = codeblocks::parse(input)?;
    input = s;
    Ok((input, ForStatement { name, source, body }))
}
