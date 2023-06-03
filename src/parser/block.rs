use crate::tokenizer::Tokenstack;

use super::{Error, PRes};

const BLOCK_PARSERS: &[fn(Tokenstack) -> PRes<Box<dyn Block>>] = &[];
pub trait Block: std::fmt::Debug {}
pub(super) fn parse_block(input: Tokenstack) -> PRes<Box<dyn Block>> {
    for p in BLOCK_PARSERS.iter() {
        match p(input) {
            Ok((s, b)) => return Ok((s, b)),
            Err(_) => {}
        }
    }
    Err(Error::Empty)
}
