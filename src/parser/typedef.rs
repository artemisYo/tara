// TypeDef <- "type" Ident GenericArgs TypeDefBody
// TypeDefBody.Struct <- "{" (Ident ":" Type ",")* (Ident ":" Type)? "}"
// TypeDefBody.Enum <- "{" ("case" Ident EnumCase ",")* ("case" Ident EnumCase)? "}"
// TypeDefBody.Singlet <- ";"
// EnumCase.Struct <- TypeDefBody.Struct
// EnumCase.Tuple <- "(" (Type ",")* Type? ")"
// EnumCase.Singlet <- ""
use crate::{
    expect,
    parser::{generics, types, Error},
    tokenizer::{Token, Tokenstack},
};

use super::{generics::GenericArgs, types::Type, PRes};

#[derive(Debug)]
pub struct TypeDef {
    name: Token,
    generics: Box<GenericArgs>,
    body: Box<TypeDefBody>,
}
#[derive(Debug)]
pub enum TypeDefBody {
    Struct(Vec<(Permissions, Token, Type)>),
    Enum(Vec<(Token, EnumCase)>),
    Singlet,
}
#[derive(Debug)]
pub enum Permissions {
    Readable,
    Writable,
    None,
}
#[derive(Debug)]
pub enum EnumCase {
    Struct(Vec<(Token, Type)>),
    Tuple(Vec<Type>),
    Singlet,
}

pub(super) fn parse(mut input: Tokenstack) -> PRes<TypeDef> {
    expect!(input, Token::TypeKey);
    let name = input.pop_if(Token::is_ident).ok_or(Error::Empty)?;
    let (s, generics) = generics::declared::parse(input)?;
    let generics = Box::new(generics);
    input = s;
    let (s, body) = parse_type_def_body(input)?;
    let body = Box::new(body);
    input = s;
    Ok((
        input,
        TypeDef {
            name,
            generics,
            body,
        },
    ))
}

fn parse_type_def_body(input: Tokenstack) -> PRes<TypeDefBody> {
    if let Ok((s, b)) = parse_type_def_body_struct(input, true) {
        Ok((s, b))
    } else if let Ok((s, b)) = parse_type_def_body_enum(input) {
        Ok((s, b))
    } else if let Ok((s, b)) = parse_type_def_body_singlet(input) {
        Ok((s, b))
    } else {
        Err(Error::Empty)
    }
}

fn parse_type_def_body_singlet(mut input: Tokenstack) -> PRes<TypeDefBody> {
    expect!(input, Token::SemiCol);
    Ok((input, TypeDefBody::Singlet))
}

fn parse_type_def_body_enum(mut input: Tokenstack) -> PRes<TypeDefBody> {
    expect!(input, Token::OpenCurly);
    let mut acc = vec![];
    // runs any amount of times
    loop {
        if input.next_is(Token::CloseCurly) {
            break;
        }
        expect!(input, Token::CaseKey);
        let n = input.pop_if(Token::is_ident).ok_or(Error::Empty)?;
        let (s, cs) = parse_enum_case(input)?;
        input = s;
        acc.push((n, cs));
        if input.pop_if(Token::Comma).is_none() || input.next_is(Token::CloseCurly) {
            break;
        }
    }
    expect!(input, Token::CloseCurly);
    Ok((input, TypeDefBody::Enum(acc)))
}

fn parse_type_def_body_struct(mut input: Tokenstack, accept_readable: bool) -> PRes<TypeDefBody> {
    expect!(input, Token::OpenCurly);
    let mut acc = vec![];
    // runs any amount of times
    loop {
        if input.next_is(Token::CloseCurly) {
            break;
        }
        let perms = if accept_readable {
            match input.peek() {
                Token::ReadableKey => {
                    input.pop();
                    Permissions::Readable
                }
                Token::WriteableKey => {
                    input.pop();
                    Permissions::Writable
                }
                _ => Permissions::None,
            }
        } else {
            Permissions::None
        };
        let n = input.pop_if(Token::is_ident).ok_or(Error::Empty)?;
        expect!(input, Token::Colon);
        let (s, ty) = types::parse(input)?;
        acc.push((perms, n, ty));
        input = s;
        if input.pop_if(Token::Comma).is_none() || input.next_is(Token::CloseCurly) {
            break;
        }
    }
    expect!(input, Token::CloseCurly);
    Ok((input, TypeDefBody::Struct(acc)))
}

fn parse_enum_case(input: Tokenstack) -> PRes<EnumCase> {
    if let Ok((s, c)) = parse_enum_case_struct(input) {
        Ok((s, c))
    } else if let Ok((s, c)) = parse_enum_case_tuple(input) {
        Ok((s, c))
    } else {
        Ok((input, EnumCase::Singlet))
    }
}

fn parse_enum_case_struct(input: Tokenstack) -> PRes<EnumCase> {
    match parse_type_def_body_struct(input, false) {
        Ok((s, a)) => match a {
            TypeDefBody::Struct(v) => Ok((
                s,
                EnumCase::Struct(v.into_iter().map(|(_, t, ty)| (t, ty)).collect()),
            )),
            _ => unreachable!(),
        },
        Err(e) => Err(e),
    }
}

fn parse_enum_case_tuple(mut input: Tokenstack) -> PRes<EnumCase> {
    expect!(input, Token::OpenParen);
    let mut acc = vec![];
    // runs any amount of times
    loop {
        if input.next_is(Token::CloseParen) {
            break;
        }
        let (s, ty) = types::parse(input)?;
        input = s;
        acc.push(ty);
        if input.pop_if(Token::Comma).is_none() || input.next_is(Token::CloseParen) {
            break;
        }
    }
    expect!(input, Token::CloseParen);
    Ok((input, EnumCase::Tuple(acc)))
}
