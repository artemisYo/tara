// Implement <- "impl" Ident GenericArgs "on" Type ImplementBody
// ImplementBody <- "{" (Ident "=" FnPath ",")* (Ident "=" FnPath)? "}"
use crate::{
    expect,
    parser::{generics, types, Error},
    tokenizer::{Token, Tokenstack},
};

use super::{generics::GenericArgs, types::Type, PRes};

#[derive(Debug)]
pub struct Implement {
    protocol: Token,
    implementor: Box<Type>,
    generics: Box<GenericArgs>,
    body: Box<ImplementBody>,
}
#[derive(Debug)]
pub struct ImplementBody(Vec<(Token, Vec<Token>)>);

pub fn parse(mut input: Tokenstack) -> PRes<Implement> {
    expect!(input, Token::ImplKey);
    let protocol = input.pop_if(Token::is_ident).ok_or(Error::Empty)?;
    let (s, generics) = generics::declared::parse(input)?;
    let generics = Box::new(generics);
    input = s;
    expect!(input, Token::OnKey);
    let (s, implementor) = types::parse(input)?;
    let implementor = Box::new(implementor);
    input = s;
    let (s, body) = parse_implement_body(input)?;
    let body = Box::new(body);
    input = s;
    Ok((
        input,
        Implement {
            protocol,
            implementor,
            generics,
            body,
        },
    ))
}

fn parse_implement_body(mut input: Tokenstack) -> PRes<ImplementBody> {
    expect!(input, Token::OpenCurly);
    let mut acc = vec![];
    // is any amount
    loop {
        if input.next_is(Token::CloseCurly) {
            break;
        }
        let n = input.pop_if(Token::is_ident).ok_or(Error::Empty)?;
        expect!(input, Token::Equals);
        let mut p = vec![];
        // is once or more
        loop {
            p.push(input.pop_if(Token::is_ident).ok_or(Error::Empty)?);
            if input.pop_if(Token::Accessor).is_none() {
                break;
            }
        }
        acc.push((n, p));
        if input.pop_if(Token::Comma).is_none() || input.next_is(Token::CloseCurly) {
            break;
        }
    }
    expect!(input, Token::CloseCurly);
    Ok((input, ImplementBody(acc)))
}
