use crate::tokenizer::Tokenstream;

mod block;
mod top_level;

pub type PRes<'a, T> = Result<(Tokenstream<'a>, T), ()>;
