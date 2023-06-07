// Type.Fn <- "fn" "(" (Type ",")* Type? ")" (":" Type)?
// Type.Plain <- "mut"? ("*" "mut"?)* "["^n Ident GenericsSupplied ((";" Num)? "]")^n
use crate::{
    expect,
    parser::{generics, Error},
    tokenizer::{Token, Tokenstack},
};

use super::{generics::GenericsSupplied, PRes};

#[derive(Debug)]
pub enum Type {
    Plain {
        name: Token,
        refs: Vec<bool>,
        arrays: Vec<Option<Token>>,
        generics: Box<GenericsSupplied>,
        is_mut: bool,
    },
    Fn {
        args: Vec<Type>,
        rety: Option<Box<Type>>,
        generics: Box<GenericsSupplied>,
    },
}

pub(super) fn parse(input: Tokenstack) -> PRes<Type> {
    if let Ok((s, a)) = parse_fn(input) {
        Ok((s, a))
    } else if let Ok((s, a)) = parse_plain(input) {
        Ok((s, a))
    } else {
        Err(Error::Empty)
    }
}

fn parse_fn(mut input: Tokenstack) -> PRes<Type> {
    expect!(input, Token::FnKey);
    let (s, generics) = generics::supplied::parse(input)?;
    let generics = Box::new(generics);
    input = s;
    expect!(input, Token::OpenParen);
    let mut args = vec![];
    // runs any amount of times
    loop {
        if input.next_is(Token::CloseParen) {
            break;
        }
        let (s, ty) = parse(input)?;
        input = s;
        args.push(ty);
        if input.pop_if(Token::Comma).is_none() || input.next_is(Token::CloseParen) {
            break;
        }
    }
    expect!(input, Token::CloseParen);
    let rety = if input.pop_if(Token::Colon).is_some() {
        let (s, ty) = parse(input)?;
        input = s;
        Some(Box::new(ty))
    } else {
        None
    };
    Ok((
        input,
        Type::Fn {
            args,
            rety,
            generics,
        },
    ))
}

fn parse_plain(mut input: Tokenstack) -> PRes<Type> {
    let is_mut = input.pop_if(Token::MutKey).is_some();
    let mut refs = vec![];
    let mut arrays = vec![];
    // runs any amount of times
    while input.pop_if(Token::Star).is_some() {
        refs.push(input.pop_if(Token::MutKey).is_some());
    }
    // runs any amount of times
    while input.pop_if(Token::OpenBracket).is_some() {
        arrays.push(None);
    }
    let name = input.pop_if(Token::is_ident).ok_or(Error::Empty)?;
    let (s, generics) = generics::supplied::parse(input)?;
    let generics = Box::new(generics);
    input = s;
    for i in 0..arrays.len() {
        arrays[i] = if input.pop_if(Token::SemiCol).is_some() {
            input.pop_if(Token::is_num)
        } else {
            None
        };
        expect!(input, Token::CloseBracket);
    }
    Ok((
        input,
        Type::Plain {
            name,
            refs,
            arrays,
            generics,
            is_mut,
        },
    ))
}
