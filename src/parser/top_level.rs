use super::block::Block;
use super::PRes;
use crate::tokenizer::{Token, Tokenstream};

macro_rules! expect {
    ($sub:expr, $test:literal) => {
        if $sub != &Token::parse($test) {
            Err(())?;
        }
    };
}

pub struct Program<'a>(Vec<Function<'a>>);
impl Program<'_> {
    pub fn parse(mut input: Tokenstream) -> PRes<Self> {
        let mut acc = vec![];
        loop {
            match Function::parse(input) {
                Ok((s, f)) => {
                    input = s;
                    acc.push(f);
                }
                Err(()) => {
                    break;
                }
            }
        }
        Ok((input, Self(acc)))
    }
}
pub struct Function<'a> {
    name: Token<'a>,
    params: Vec<(Token<'a>, Type<'a>)>,
    ret_type: Option<Token<'a>>,
    body: Box<dyn Block>,
}
impl Function<'_> {
    fn parse(mut input: Tokenstream) -> PRes<Self> {
        expect!(input.peek().ok_or(())?, "fn");
        input.next();
        let name = match input.peek().ok_or(())? {
            Token::Ident(n) => input.next().unwrap(),
            _ => Err(())?,
        };
        expect!(input.peek().ok_or(())?, "(");
        input.next();
        todo!("parse args for fn");
        expect!(input.peek().ok_or(())?, ")");
        input.next();
        todo!("parse body and return type of fn");
    }
}
pub struct Type<'a> {
    name: Token<'a>,
    params: Vec<Type<'a>>,
    array: Vec<usize>,
    refs: usize,
}
