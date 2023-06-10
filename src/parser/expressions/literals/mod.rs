use crate::tokenizer::Tokenstack;

use super::{Expression, PRes};

mod num;

pub trait Literal: std::fmt::Debug + Expression {}
pub fn parse(mut input: Tokenstack) -> PRes<Box<dyn Literal>> {
    let (s, n) = num::parse(input)?;
    Ok((s, Box::new(n)))
}
