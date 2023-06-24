use crate::{tokenizer::Tokenstack};

use super::{executable::Executable, PRes, Error};

pub trait CodeBlock: std::fmt::Debug + Executable {}

mod forstatement;
mod ifstatement;
mod matchstatement;
mod plainblock;

const NAME: &'static str = "CodeBlock";
pub fn parse(input: Tokenstack) -> PRes<Box<dyn CodeBlock>> {
    let mut errors = vec![];
    match plainblock::parse(input) {
        Ok((s, a)) => return Ok((s, Box::new(a))),
        Err(e) => errors.push(e),
    }
    match matchstatement::parse(input) {
        Ok((s, a)) => return Ok((s, Box::new(a))),
        Err(e) => errors.push(e),
    }
    match ifstatement::parse(input) {
        Ok((s, a)) => return Ok((s, Box::new(a))),
        Err(e) => errors.push(e),
    }
    match forstatement::parse(input) {
        Ok((s, a)) => return Ok((s, Box::new(a))),
        Err(e) => errors.push(e),
    }
    Err(Error::Choice { errors }.trace(NAME))
}
