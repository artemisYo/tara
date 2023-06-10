// MatchStatement <- "match" Expression "{" MatchCase* "}"
// MatchCase <- "case" Literal PlainBlock
use crate::{
    expect,
    parser::{
        codeblocks::plainblock,
        executable::Executable,
        expressions::{
            self,
            literals::{self, Literal},
            Expression,
        },
        PRes,
    },
    tokenizer::{Token, Tokenstack},
};

use super::CodeBlock;

#[derive(Debug)]
pub struct MatchStatement {
    source: Box<dyn Expression>,
    patterns: Vec<MatchCase>,
}
#[derive(Debug)]
pub struct MatchCase {
    pattern: Box<dyn Literal>,
    body: Box<dyn CodeBlock>,
}
impl Executable for MatchStatement {
    fn as_executable(self: Box<Self>) -> Box<dyn Executable> {
        self
    }
}
impl CodeBlock for MatchStatement {}
pub fn parse(mut input: Tokenstack) -> PRes<MatchStatement> {
    expect!(input, Token::MatchKey);
    let (s, source) = expressions::parse(input)?;
    input = s;
    expect!(input, Token::OpenCurly);
    let mut patterns = vec![];
    // runs any amount of times
    loop {
        if input.next_is(Token::CloseCurly) {
            break;
        }
        let (s, p) = parse_match_case(input)?;
        patterns.push(p);
        input = s;
    }
    expect!(input, Token::CloseCurly);
    Ok((input, MatchStatement { source, patterns }))
}
fn parse_match_case(mut input: Tokenstack) -> PRes<MatchCase> {
    expect!(input, Token::CaseKey);
    let (s, pattern) = literals::parse(input)?;
    input = s;
    let (s, body) = plainblock::parse(input)?;
    let body = Box::new(body);
    input = s;
    Ok((input, MatchCase { pattern, body }))
}
