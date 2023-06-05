use crate::expect;
use crate::tokenizer::{Token, Tokenstack};

use super::{expr::Expression, Error, PRes};

#[derive(Debug)]
pub enum Block {
    BlockExpr(Box<BlockExpr>),
    PlainBlock(Box<Vec<Expression>>, bool),
}
impl Block {
    pub(super) fn parse(mut input: Tokenstack) -> PRes<Self> {
        match input.peek() {
            Token::DoKey => parse_plain_block(input),
            _ => match BlockExpr::parse(input) {
                Ok((s, a)) => Ok((s, Self::BlockExpr(Box::new(a)))),
                Err(e) => Err(e),
            },
        }
    }
}

fn parse_plain_block(mut input: Tokenstack) -> PRes<Block> {
    // "do"
    // TODO: expect!(input, Token::DoKey);
    expect!(input, Token::Arrow);
    // (Expression ",")* Expression
    let mut acc = Box::new(vec![]);
    loop {
        let last_block;
        let (s, a) = match BlockExpr::parse(input) {
            Ok((s, a)) => {
                last_block = true;
                (s, Expression::BlockExpr(a))
            }
            Err(_) => {
                last_block = false;
                Expression::parse(input)?
            }
        };
        acc.push(a);
        input = s;
        match input.peek() {
            Token::EndKey => break,
            Token::Comma => {
                input.pop();
            }
            _ if last_block => {}
            _ => break,
        }
    }
    // "end"
    // TODO: expect!(input, Token::EndKey);
    // ("." | ";")
    let returns;
    match input.pop() {
        Token::Period => {
            returns = true;
        }
        Token::SemiCol => {
            returns = false;
        }
        _ => {
            return Err(Error::Multiple {
                expected: vec![Token::Period, Token::SemiCol],
                found: input.pop(),
                origin: input,
            });
        }
    }
    Ok((input, Block::PlainBlock(acc, returns)))
}

#[derive(Debug)]
pub enum BlockExpr {
    MatchStmt {
        cases: Vec<MatchCase>,
        source: Box<Expression>,
    },
    IfStmt {
        cond: Box<Expression>,
        smash: Box<Block>,
        pass: Option<Box<Block>>,
    },
    ForStmt {
        ident: Token,
        source: Box<Expression>,
        body: Box<Block>,
    },
}
impl BlockExpr {
    pub(super) fn parse(mut input: Tokenstack) -> PRes<Self> {
        match input.peek() {
            Token::MatchKey => parse_match_stmt(input),
            Token::IfKey => parse_if_stmt(input),
            Token::ForKey => parse_for_stmt(input),
            _ => Err(Error::Multiple {
                expected: vec![Token::MatchKey, Token::IfKey, Token::ForKey],
                found: input.pop(),
                origin: input,
            }),
        }
    }
}

fn parse_match_stmt(mut input: Tokenstack) -> PRes<BlockExpr> {
    // "match"
    expect!(input, Token::MatchKey);
    // Expression
    let (s, source) = super::expr::Expression::parse(input)?;
    let source = Box::new(source);
    input = s;
    // "->"
    expect!(input, Token::Arrow);
    // MatchCases+
    let mut cases = vec![];
    while let Ok((s, a)) = MatchCase::parse(input) {
        input = s;
        cases.push(a);
    }
    if cases.is_empty() {
        return Err(Error::Expected {
            expected: Token::CaseKey,
            found: input.pop(),
            origin: input,
        });
    }
    Ok((input, BlockExpr::MatchStmt { cases, source }))
}

#[derive(Debug)]
pub struct MatchCase {
    pattern: Box<Expression>,
    smash: Box<Block>,
}
impl MatchCase {
    fn parse(mut input: Tokenstack) -> PRes<Self> {
        // "case"
        expect!(input, Token::CaseKey);
        // Expression Block
        let (s, pattern) = super::expr::Expression::parse(input)?;
        let pattern = Box::new(pattern);
        input = s;
        let (s, smash) = Block::parse(input)?;
        let smash = Box::new(smash);
        input = s;
        Ok((input, Self { pattern, smash }))
    }
}

fn parse_if_stmt(mut input: Tokenstack) -> PRes<BlockExpr> {
    // "if"
    expect!(input, Token::IfKey);
    // Expression
    let (s, cond) = super::expr::Expression::parse(input)?;
    let cond = Box::new(cond);
    input = s;
    // Block
    let (s, smash) = Block::parse(input)?;
    let smash = Box::new(smash);
    input = s;
    // ("else" Block)?
    let pass = if input.peek() == &Token::ElseKey {
        input.pop();
        let (s, a) = Block::parse(input)?;
        input = s;
        Some(Box::new(a))
    } else {
        None
    };
    Ok((input, BlockExpr::IfStmt { cond, smash, pass }))
}

fn parse_for_stmt(mut input: Tokenstack) -> PRes<BlockExpr> {
    // "for"
    expect!(input, Token::ForKey);
    // Ident
    let ident = input.pop();
    if !ident.is_ident() {
        return Err(Error::Expected {
            expected: Token::Ident("".to_string()),
            found: input.pop(),
            origin: input,
        });
    }
    // "in"
    expect!(input, Token::InKey);
    // Expression
    let (s, source) = super::expr::Expression::parse(input)?;
    let source = Box::new(source);
    input = s;
    let (s, body) = Block::parse(input)?;
    let body = Box::new(body);
    input = s;
    Ok((
        input,
        BlockExpr::ForStmt {
            ident,
            source,
            body,
        },
    ))
}
