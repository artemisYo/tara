use super::{Error, PRes};
use crate::tokenizer::Tokenstack;

#[derive(Debug)]
pub enum Expression {
    BlockExpr(super::block::BlockExpr),
}
impl Expression {
    pub(super) fn parse(mut input: Tokenstack) -> PRes<Self> {
        todo!()
    }
}
