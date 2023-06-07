pub use self::{declared::GenericArgs, supplied::GenericsSupplied};
pub mod declared {
    // GenericArgs <- ("[" (Ident Constraints ",")* (Ident Constraints ","?)? "]")?
    // Constraints <- (":" (Ident GenericsSupplied "+")* Ident GenericsSupplied)?
    use crate::{
        expect,
        tokenizer::{Token, Tokenstack},
    };

    use super::{
        super::{Error, PRes},
        GenericsSupplied,
    };
    #[derive(Debug)]
    pub struct GenericArgs(Vec<(Token, Constraints)>);
    #[derive(Debug)]
    pub struct Constraints(Vec<(Token, GenericsSupplied)>);

    pub(in super::super) fn parse(mut input: Tokenstack) -> PRes<GenericArgs> {
        let mut acc = vec![];
        if input.pop_if(Token::OpenBracket).is_some() {
            // run any amount of times
            loop {
                if input.next_is(Token::CloseBracket) {
                    break;
                }
                let name = input.pop_if(Token::is_ident).ok_or(Error::Empty)?;
                let (s, constraints) = parse_constraint(input)?;
                input = s;
                acc.push((name, constraints));
                if input.pop_if(Token::Comma).is_none() || input.next_is(Token::CloseBracket) {
                    break;
                }
            }
            expect!(input, Token::CloseBracket);
        }
        Ok((input, GenericArgs(acc)))
    }

    fn parse_constraint(mut input: Tokenstack) -> PRes<Constraints> {
        let mut acc = vec![];
        if input.pop_if(Token::Colon).is_some() {
            // runs atleast once
            loop {
                let t = input.pop_if(Token::is_ident).ok_or(Error::Empty)?;
                let (s, p) = super::supplied::parse(input)?;
                input = s;
                acc.push((t, p));
                if input.pop_if(Token::Plus).is_none() {
                    break;
                }
            }
        }
        Ok((input, Constraints(acc)))
    }
}
pub mod supplied {
    // GenericsSupplied <- ("[" (Type ",")* (Type ","?)? "]")?
    use crate::{
        expect,
        parser::types,
        tokenizer::{Token, Tokenstack},
    };

    use super::super::{types::Type, PRes};
    #[derive(Debug)]
    pub struct GenericsSupplied(Vec<Type>);
    pub(in super::super) fn parse(mut input: Tokenstack) -> PRes<GenericsSupplied> {
        let mut acc = vec![];
        if input.pop_if(Token::OpenBracket).is_some() {
            // runs any amount of times
            loop {
                if input.next_is(Token::CloseBracket) {
                    break;
                }
                let (s, ty) = types::parse(input)?;
                input = s;
                acc.push(ty);
                if input.pop_if(Token::Comma).is_none() || input.next_is(Token::CloseBracket) {
                    break;
                }
            }
            expect!(input, Token::CloseBracket);
        }
        Ok((input, GenericsSupplied(acc)))
    }
}
