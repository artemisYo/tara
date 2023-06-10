// Protocol <- "proto" Ident GenericArgs "on" Ident ProtocolBody
// ProtocolBody <- "{" (Ident ":" Type ",")* (Ident ":" Type ","?)? "}"
use crate::{
    expect,
    parser::Error,
    tokenizer::{Token, Tokenstack},
};

use super::{generics, generics::GenericArgs, types::Type, PRes};

#[derive(Debug)]
pub struct Protocol {
    name: Token,
    implementor: Token,
    generics: Box<GenericArgs>,
    body: Box<ProtocolBody>,
}

#[derive(Debug)]
pub struct ProtocolBody {
    fields: Vec<(Token, Type)>,
}

pub fn parse(mut input: Tokenstack) -> PRes<Protocol> {
    expect!(input, Token::ProtoKey);
    let name = input.pop_if(Token::is_ident).ok_or(Error::Empty)?;
    let (s, generics) = generics::declared::parse(input)?;
    let generics = Box::new(generics);
    input = s;
    expect!(input, Token::OnKey);
    let implementor = input.pop_if(Token::is_ident).ok_or(Error::Empty)?;
    let (input, body) = parse_protocol_body(input)?;
    let body = Box::new(body);
    Ok((
        input,
        Protocol {
            name,
            implementor,
            generics,
            body,
        },
    ))
}

fn parse_protocol_body(mut input: Tokenstack) -> PRes<ProtocolBody> {
    expect!(input, Token::OpenCurly);
    let mut fields = vec![];
    // runs any amount of times
    loop {
        if input.next_is(Token::CloseCurly) {
            break;
        }
        let name = input.pop_if(Token::is_ident).ok_or(Error::Empty)?;
        expect!(input, Token::Colon);
        let (s, ty) = super::types::parse(input)?;
        input = s;
        fields.push((name, ty));
        if input.pop_if(Token::Comma).is_none() || input.next_is(Token::CloseCurly) {
            break;
        }
    }
    expect!(input, Token::CloseCurly);
    Ok((input, ProtocolBody { fields }))
}
