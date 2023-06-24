// IfStatement <- "if" Executable CodeBlock ("else" CodeBlock)?
use crate::{
    expect,
    parser::{
        codeblocks,
        executable::{self, Executable},
        patterns::Pattern,
        PRes, Traceable,
    },
    tokenizer::{Token, Tokenstack},
};

use super::CodeBlock;

#[derive(Debug)]
pub struct IfStatement {
    cond: Box<dyn Executable>,
    smash: Box<dyn CodeBlock>,
    pass: Option<Box<dyn CodeBlock>>,
}
impl Pattern for IfStatement {
    fn as_pattern(self: Box<Self>) -> Box<dyn Pattern> {
        self
    }
}
impl CodeBlock for IfStatement {}
impl Executable for IfStatement {
    fn as_executable(self: Box<Self>) -> Box<dyn Executable> {
        self
    }
}

const NAME: &'static str = "IfStatement";
pub fn parse(mut input: Tokenstack) -> PRes<impl CodeBlock> {
    expect!(input, Token::IfKey);
    let (s, cond) = executable::parse(input).trace(NAME)?;
    input = s;
    let (s, smash) = codeblocks::parse(input).trace(NAME)?;
    input = s;
    let pass: Option<Box<dyn CodeBlock>> = if input.pop_if(Token::ElseKey).is_some() {
        let (s, b) = codeblocks::parse(input).trace(NAME)?;
        input = s;
        Some(b)
    } else {
        None
    };
    Ok((input, IfStatement { cond, smash, pass }))
}
