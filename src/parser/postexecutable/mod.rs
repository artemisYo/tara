use crate::tokenizer::Tokenstack;

use super::{executable::Executable, Error};

type PERes<'a, T> = Result<(Tokenstack<'a>, T), (Box<dyn Executable>, Error<'a>)>;

pub mod arithmetic;
mod assignment;
mod call;
mod cmp;
mod fieldaccess;
mod indexaccess;
mod opassign;
mod quickinc;
mod range;

pub trait PostExecutable: Executable + std::fmt::Debug {}

const NAME: &'static str = "PostExecutable";
pub fn parse(input: Tokenstack, mut exec: Box<dyn Executable>) -> PERes<Box<dyn Executable>> {
    let mut errors = vec![];
    match quickinc::parse(input, exec) {
        Ok((s, a)) => {
            return Ok((s, Box::new(a)));
        }
        Err((ex, e)) => {
            errors.push(e);
            exec = ex;
        }
    }
    match assignment::parse(input, exec) {
        Ok((s, a)) => {
            return Ok((s, Box::new(a)));
        }
        Err((ex, e)) => {
            errors.push(e);
            exec = ex;
        }
    }
    match cmp::parse(input, exec) {
        Ok((s, a)) => {
            return Ok((s, Box::new(a)));
        }
        Err((ex, e)) => {
            errors.push(e);
            exec = ex;
        }
    }
    match opassign::parse(input, exec) {
        Ok((s, a)) => {
            return Ok((s, Box::new(a)));
        }
        Err((ex, e)) => {
            errors.push(e);
            exec = ex;
        }
    }
    match arithmetic::parse(input, exec) {
        Ok((s, a)) => {
            return Ok((s, a.as_executable()));
        }
        Err((ex, e)) => {
            errors.push(e);
            exec = ex;
        }
    }
    match call::parse(input, exec) {
        Ok((s, a)) => {
            return Ok((s, Box::new(a)));
        }
        Err((ex, e)) => {
            errors.push(e);
            exec = ex;
        }
    }
    match fieldaccess::parse(input, exec) {
        Ok((s, a)) => {
            return Ok((s, Box::new(a)));
        }
        Err((ex, e)) => {
            errors.push(e);
            exec = ex;
        }
    }
    match indexaccess::parse(input, exec) {
        Ok((s, a)) => {
            return Ok((s, Box::new(a)));
        }
        Err((ex, e)) => {
            errors.push(e);
            exec = ex;
        }
    }
    match range::parse(input, exec) {
        Ok((s, a)) => {
            return Ok((s, Box::new(a)));
        }
        Err((ex, e)) => {
            errors.push(e);
            exec = ex;
        }
    }
    Err((exec, Error::Choice { errors }.trace(NAME)))
}
