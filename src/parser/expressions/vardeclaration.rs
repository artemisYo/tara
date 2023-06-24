// VariableDeclaration.mutable <- "mut" Ident (":" Type)? "=" Executable
// VariableDeclaration.immutable <- "let" Ident (":" Type)? "=" Executable
use crate::{
    expect,
    parser::{
        executable::{self, Executable},
        patterns::Pattern,
        types::{self, Type},
        Error, PRes, Traceable,
    },
    tokenizer::{Token, Tokenstack},
};

use super::Expression;

#[derive(Debug)]
pub struct VariableDeclaration {
    name: Token,
    ty: Option<Box<Type>>,
    source: Box<dyn Executable>,
    is_mutable: bool,
}

impl Pattern for VariableDeclaration {
    fn as_pattern(self: Box<Self>) -> Box<dyn Pattern> {
        self
    }
}
impl Expression for VariableDeclaration {
    fn as_expression(self: Box<Self>) -> Box<dyn Expression> {
        self
    }
}
impl Executable for VariableDeclaration {
    fn as_executable(self: Box<Self>) -> Box<dyn Executable> {
        self
    }
}
const NAME: &'static str = "VariableDeclaration";
pub fn parse(mut input: Tokenstack) -> PRes<VariableDeclaration> {
    let is_mutable = match input.pop() {
        Token::MutKey => true,
        Token::LetKey => false,
        _ => Err(Error::Multiple { possible: &[Token::MutKey, Token::LetKey], origin: input })?,
    };
    let name = input.pop_if(Token::is_ident).ok_or_else(|| Error::Expected { expected: Token::Ident("".to_owned()), origin: input })?;
    let ty = if input.pop_if(Token::Colon).is_some() {
        let (s, t) = types::parse(input).trace(NAME)?;
        input = s;
        Some(Box::new(t))
    } else {
        None
    };
    expect!(input, Token::Equals);
    let (s, source) = executable::parse(input).trace(NAME)?;
    input = s;
    Ok((
        input,
        VariableDeclaration {
            name,
            ty,
            source,
            is_mutable,
        },
    ))
}
