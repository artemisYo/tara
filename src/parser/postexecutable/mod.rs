use crate::tokenizer::Tokenstack;

use super::{executable::Executable, Error};

type PERes<'a, T> = Result<(Tokenstack<'a>, T), (Box<dyn Executable>, Error<'a>)>;

pub mod arithmetic;
mod assignment;
mod call;
mod fieldaccess;
mod indexaccess;
mod range;

pub trait PostExecutable: Executable + std::fmt::Debug {}

pub fn parse(input: Tokenstack, mut exec: Box<dyn Executable>) -> PERes<Box<dyn Executable>> {
    match arithmetic::parse(input, exec) {
        Ok((s, a)) => {
            return Ok((s, a.as_executable()));
        }
        Err((ex, _)) => {
            exec = ex;
        }
    }
    match call::parse(input, exec) {
        Ok((s, a)) => {
            return Ok((s, Box::new(a)));
        }
        Err((ex, _)) => {
            exec = ex;
        }
    }
    match assignment::parse(input, exec) {
        Ok((s, a)) => {
            return Ok((s, Box::new(a)));
        }
        Err((ex, _)) => {
            exec = ex;
        }
    }
    match fieldaccess::parse(input, exec) {
        Ok((s, a)) => {
            return Ok((s, Box::new(a)));
        }
        Err((ex, _)) => {
            exec = ex;
        }
    }
    match indexaccess::parse(input, exec) {
        Ok((s, a)) => {
            return Ok((s, Box::new(a)));
        }
        Err((ex, _)) => {
            exec = ex;
        }
    }
    match range::parse(input, exec) {
        Ok((s, a)) => {
            return Ok((s, Box::new(a)));
        }
        Err((ex, _)) => {
            exec = ex;
        }
    }
    Err((exec, Error::Empty))
}
