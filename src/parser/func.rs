// Function.Plain <- "fn" Ident GenericArgs FunctionArgs (":" Type)? CodeBlock
// Function.Method <- "fn" Ident GenericArgs "on" Type FunctionArgs (":" Type)? CodeBlock
// Function.Associated <- "fn" Ident GenericArgs "for" Type FunctionArgs (":" Type)? CodeBlock
// FunctionArgs <- "(" (Ident ":" Type ",")* (Ident ":" Type)? ")"
use crate::{
    expect,
    parser::{codeblocks, generics, types, Error},
    tokenizer::{Token, Tokenstack},
};

use super::{codeblocks::CodeBlock, generics::GenericArgs, types::Type, PRes};
#[derive(Debug)]
pub enum Function {
    Plain {
        name: Token,
        generics: Box<GenericArgs>,
        args: Box<FunctionArgs>,
        rety: Option<Box<Type>>,
        body: Box<dyn CodeBlock>,
    },
    Method {
        name: Token,
        object: Box<Type>,
        generics: Box<GenericArgs>,
        args: Box<FunctionArgs>,
        rety: Option<Box<Type>>,
        body: Box<dyn CodeBlock>,
    },
    Associated {
        name: Token,
        object: Box<Type>,
        generics: Box<GenericArgs>,
        args: Box<FunctionArgs>,
        rety: Option<Box<Type>>,
        body: Box<dyn CodeBlock>,
    },
}
#[derive(Debug)]
pub struct FunctionArgs(Vec<(Token, Type)>);

pub fn parse(mut input: Tokenstack) -> PRes<Function> {
    enum FuncType {
        Plain,
        Method,
        Associated,
    }
    let functype;
    expect!(input, Token::FnKey);
    let name = input.pop_if(Token::is_ident).ok_or(Error::Empty)?;
    let (s, generics) = generics::declared::parse(input)?;
    let generics = Box::new(generics);
    input = s;
    let mut object = None;
    match input.peek() {
        Token::OnKey => {
            input.pop();
            functype = FuncType::Method;
            let (s, obj) = types::parse(input)?;
            object = Some(Box::new(obj));
            input = s;
        }
        Token::ForKey => {
            input.pop();
            functype = FuncType::Associated;
            let (s, obj) = types::parse(input)?;
            object = Some(Box::new(obj));
            input = s;
        }
        _ => {
            functype = FuncType::Plain;
        }
    }
    let (s, args) = parse_function_args(input)?;
    let args = Box::new(args);
    input = s;
    let rety = if input.pop_if(Token::Colon).is_some() {
        let (s, ty) = types::parse(input)?;
        input = s;
        Some(Box::new(ty))
    } else {
        None
    };
    let (s, body) = codeblocks::parse(input)?;
    input = s;
    match functype {
        FuncType::Plain => Ok((
            input,
            Function::Plain {
                name,
                generics,
                args,
                rety,
                body,
            },
        )),
        FuncType::Method => Ok((
            input,
            Function::Method {
                name,
                object: object.unwrap(),
                generics,
                args,
                rety,
                body,
            },
        )),
        FuncType::Associated => Ok((
            input,
            Function::Associated {
                name,
                object: object.unwrap(),
                generics,
                args,
                rety,
                body,
            },
        )),
    }
}
fn parse_function_args(mut input: Tokenstack) -> PRes<FunctionArgs> {
    expect!(input, Token::OpenParen);
    let mut acc = vec![];
    // runs any amount of times
    loop {
        if input.next_is(Token::CloseParen) {
            break;
        }
        let n = input.pop_if(Token::is_ident).ok_or(Error::Empty)?;
        expect!(input, Token::Colon);
        let (s, ty) = types::parse(input)?;
        acc.push((n, ty));
        input = s;
        if input.pop_if(Token::Comma).is_none() || input.next_is(Token::CloseParen) {
            break;
        }
    }
    expect!(input, Token::CloseParen);
    Ok((input, FunctionArgs(acc)))
}
