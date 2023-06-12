use crate::tokenizer::Tokenstack;

use super::{executable, Error, PRes};

pub trait Pattern: std::fmt::Debug {
    fn as_pattern(self: Box<Self>) -> Box<dyn Pattern>;
}

pub fn parse(mut input: Tokenstack) -> PRes<Box<dyn Pattern>> {
    if let Ok((s, a)) = executable::parse(input) {
        Ok((s, a.as_pattern()))
    } else {
        Err(Error::Empty)
    }
}
