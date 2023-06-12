use crate::tokenizer::Tokenstack;

mod codeblocks;
mod executable;
mod expressions;
mod func;
mod generics;
mod implements;
mod patterns;
mod postexecutable;
mod protocol;
mod root;
mod typedef;
mod types;
pub use root::parse;

#[derive(Debug)]
pub enum Error<'a> {
    Expected {
        expected: crate::tokenizer::Token,
        found: crate::tokenizer::Token,
        origin: Tokenstack<'a>,
    },
    Multiple {
        expected: Vec<crate::tokenizer::Token>,
        found: crate::tokenizer::Token,
        origin: Tokenstack<'a>,
    },
    Empty,
}

#[macro_export]
macro_rules! expect {
    ($source:ident, $pattern:expr) => {
        $source
            .pop_if($pattern)
            .ok_or(crate::parser::Error::Empty)?;
    };
}

#[macro_export]
macro_rules! choose_first {
    ($input:ident, $($parser:path),+) => {
        $(if let Ok((s, a)) = $parser($input) {Ok((s, Box::new(a)))} else)+
        {Err(crate::parser::Error::Empty)}
    };
}

pub(in crate::parser) type PRes<'a, T> = Result<(Tokenstack<'a>, T), Error<'a>>;
