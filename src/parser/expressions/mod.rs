use crate::tokenizer::Tokenstack;

use super::{executable::Executable, Error, PRes};

mod ident;
pub mod literals;
mod references;
mod vardeclaration;

pub trait Expression: std::fmt::Debug + Executable {
    fn as_expression(self: Box<Self>) -> Box<dyn Expression>;
}

const NAME: &'static str = "Expression";
pub fn parse(input: Tokenstack) -> PRes<Box<dyn Expression>> {
    let mut errors = vec![];
    match literals::parse(input) {
        Ok((s, a)) => return Ok((s, a.as_expression())),
        Err(e) => errors.push(e),
    }
    match ident::parse(input) {
        Ok((s, a)) => return Ok((s, Box::new(a))),
        Err(e) => errors.push(e),
    }
    match vardeclaration::parse(input) {
        Ok((s, a)) => return Ok((s, Box::new(a))),
        Err(e) => errors.push(e),
    }
    match references::deref::parse(input) {
        Ok((s, a)) => return Ok((s, Box::new(a))),
        Err(e) => errors.push(e),
    }
    match references::refer::parse(input) {
        Ok((s, a)) => return Ok((s, Box::new(a))),
        Err(e) => errors.push(e),
    }

    Err(Error::Choice { errors }.trace(NAME))
}
