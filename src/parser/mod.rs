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

pub enum Error<'a> {
    Expected {
        expected: crate::tokenizer::Token,
        origin: Tokenstack<'a>,
    },
    Multiple {
        possible: &'static [crate::tokenizer::Token],
        origin: Tokenstack<'a>,
    },
    Trace {
        error: Box<Self>,
        func: &'static str,
    },
    Choice {
        errors: Vec<Self>
    },
    Empty,
}

impl<'a> Error<'a> {
    fn trace(self, func: &'static str) -> Self {
        Self::Trace { error: Box::new(self), func }
    }
}

impl<'a> std::fmt::Debug for Error<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Expected { expected, origin } => f.debug_struct("Expected").field("expected", expected).field("found", origin.peek()).finish(),
            Self::Multiple { possible, origin } => f.debug_struct("Multiple").field("possible", possible).field("found", origin.peek()).finish(),
            Self::Trace { error, func } => f.debug_struct("Trace").field("error", error).field("func", func).finish(),
            Self::Choice { errors } => f.debug_struct("Choice").field("errors", errors).finish(),
            Self::Empty => write!(f, "Empty"),
        }
    }
}

#[macro_export]
macro_rules! expect {
    ($source:ident, $pattern:expr) => {
        $source
            .pop_if($pattern)
            .ok_or_else(|| crate::parser::Error::Expected {expected: $pattern, origin: $source})?;
    };
}


trait Traceable {
    fn trace(self, rule: &'static str) -> Self;
}

pub(in crate::parser) type PRes<'a, T> = Result<(Tokenstack<'a>, T), Error<'a>>;

impl<'a, T> Traceable for PRes<'a, T> {
    fn trace(self, rule: &'static str) -> Self {
        self.map_err(|e| e.trace(rule))
    }
}