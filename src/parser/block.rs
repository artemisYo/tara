use crate::tokenizer::Tokenstream;

use super::PRes;

const BLOCK_PARSERS: &[fn(Tokenstream) -> PRes<Box<dyn Block>>] = &[];
pub trait Block {}
pub fn parse_block(input: Tokenstream) -> PRes<Box<dyn Block>> {
    for p in BLOCK_PARSERS.iter() {
        match p(input) {
            Ok((s, b)) => return Ok((s, b)),
            Err(()) => {}
        }
    }
    Err(())
}
