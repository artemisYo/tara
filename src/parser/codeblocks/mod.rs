use crate::{choose_first, tokenizer::Tokenstack};

use super::{executable::Executable, PRes};

pub trait CodeBlock: std::fmt::Debug + Executable {}

mod forstatement;
mod ifstatement;
mod matchstatement;
mod plainblock;

pub fn parse(input: Tokenstack) -> PRes<Box<dyn CodeBlock>> {
    choose_first!(
        input,
        plainblock::parse,
        matchstatement::parse,
        ifstatement::parse,
        forstatement::parse
    )
}
