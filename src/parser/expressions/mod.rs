use crate::{choose_first, tokenizer::Tokenstack};

use super::{executable::Executable, Error, PRes};

mod deref;
mod ident;
pub mod literals;
mod vardeclaration;

pub trait Expression: std::fmt::Debug + Executable {
    fn as_expression(self: Box<Self>) -> Box<dyn Expression>;
}
pub fn parse(mut input: Tokenstack) -> PRes<Box<dyn Expression>> {
    if let Ok((s, a)) = literals::parse(input) {
        Ok((s, a.as_expression()))
    } else if let Ok((s, a)) = ident::parse(input) {
        Ok((s, Box::new(a)))
    } else if let Ok((s, a)) = vardeclaration::parse(input) {
        Ok((s, Box::new(a)))
    } else if let Ok((s, a)) = deref::parse(input) {
        Ok((s, Box::new(a)))
    } else {
        Err(Error::Empty)
    }
}
