use crate::tokenizer::Tokenstack;

mod block;
mod expr;
mod top_level;
pub use top_level::Program;

#[derive(Debug)]
pub enum Error<'a> {
    Expected {
        expected: crate::tokenizer::Token,
        found: crate::tokenizer::Token,
        origin: Tokenstack<'a>,
    },
    Empty,
}

#[macro_export]
macro_rules! expect {
    ($source:ident, $pattern:expr) => {
        let t = $source.pop();
        if &t != &$pattern {
            return Err(crate::parser::Error::Expected {
                expected: $pattern,
                found: t,
                origin: $source,
            });
        }
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
