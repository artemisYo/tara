use crate::tokenizer::Tokenstack;

use super::{codeblocks, expressions, postexecutable, Error, PRes};

pub trait Executable: std::fmt::Debug {
    fn as_executable(self: Box<Self>) -> Box<dyn Executable>;
}
pub fn parse(input: Tokenstack) -> PRes<Box<dyn Executable>> {
    // PostExecutable is an Executable but is not parsed under it
    // as it is parsed after an Executable, it is a term of left factoring
    if let Ok((s, a)) = codeblocks::parse(input) {
        let mut input = s;
        let mut exec = a.as_executable();
        loop {
            match postexecutable::parse(input, exec) {
                Ok((s, a)) => {
                    input = s;
                    exec = a;
                }
                Err((a, _)) => {
                    exec = a;
                    break;
                }
            }
        }
        Ok((input, exec))
    } else if let Ok((s, a)) = expressions::parse(input) {
        let mut input = s;
        let mut exec = a.as_executable();
        loop {
            match postexecutable::parse(input, exec) {
                Ok((s, a)) => {
                    input = s;
                    exec = a;
                }
                Err((a, _)) => {
                    exec = a;
                    break;
                }
            }
        }
        Ok((input, exec))
    } else {
        Err(Error::Empty)
    }
}
