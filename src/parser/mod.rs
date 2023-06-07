use crate::tokenizer::Tokenstack;

mod generics;
mod implements;
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

pub(in crate::parser) type PRes<'a, T> = Result<(Tokenstack<'a>, T), Error<'a>>;

pub(in crate::parser) fn list<T>(
    mut input: Tokenstack,
    f: impl Fn(Tokenstack) -> PRes<T>,
) -> PRes<Vec<T>> {
    let mut acc = vec![];
    while let Ok((s, a)) = f(input) {
        input = s;
        acc.push(a);
        match input.peek() {
            crate::tokenizer::Token::Comma => {
                input.pop();
            }
            _ => {
                break;
            }
        }
    }
    Ok((input, acc))
}
