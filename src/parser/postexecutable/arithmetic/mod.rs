use crate::{
    parser::{executable::Executable, Error},
    tokenizer::Tokenstack,
};

use super::{PERes, PostExecutable};
mod plusandminus;

pub trait Arithmetic: std::fmt::Debug + PostExecutable {}
pub fn parse(mut input: Tokenstack, exec: Box<dyn Executable>) -> PERes<Box<dyn Arithmetic>> {
    match plusandminus::parse(input, exec) {
        Ok((s, a)) => Ok((s, Box::new(a))),
        Err((exec, _)) => Err((exec, Error::Empty)),
    }
}
