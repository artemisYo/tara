use crate::lexer::{Token, TokenIter};
use super::*;

pub fn parse<'a>(input: &'a [Token<'a>]) -> Option<Root> {
    Expr::parse(TokenIter::new(input)).map(|(_, e)| e)
}

type Parser<T> = for <'a> fn(TokenIter<'a>) -> Option<(TokenIter<'a>, T)>;

impl Expr {
    // Expr::parse -> Expr::parse_level(0)
    fn parse<'a>(input: TokenIter<'a>) -> Option<(TokenIter<'a>, Self)> {
        Self::parse_level(input, 0)
    }
    // Expr::parse_level(p) -> EXPRS[p]
    fn parse_level<'a>(input: TokenIter<'a>, level: usize) -> Option<(TokenIter<'a>, Self)> {
        const EXPRS: &[&[Parser<Expr>]] = &[
            &[BinExpr::parse::<0>],
            &[BinExpr::parse::<1>],
            &[Expr::parse_parens, SinExpr::parse],
        ];
        EXPRS[level].iter().map(|f| f(input)).find(Option::is_some).flatten()
    }
    // Expr::parse_parens -> "(" Expr::parse ")"
    fn parse_parens(mut input: TokenIter) -> Option<(TokenIter, Self)> {
        input.consume(Token::OpenParen)?;
        let (mut input, e) = Self::parse(input)?;
        input.consume(Token::CloseParen)?;
        Some((input, e))
    }
}

impl BinExpr {
    // BinExpr::parse<L> -> Expr::parse_level(L + 1) { BINOP[L] Expr::parse_level(L) }?
    fn parse<'a, const L: usize>(input: TokenIter<'a>) -> Option<(TokenIter<'a>, Expr)> {
        const BINOP: &[&[Token]] = &[
            &[Token::Plus, Token::Minus],
            &[Token::Star, Token::Slash],
        ];
        let (mut input, lhs) = Expr::parse_level(input, L + 1)?;
        let Some(op) = BINOP[L].iter()
            .find(|t| input.consume(**t).is_some())
            .map(|t| BinOp::from_token(*t).unwrap())
        else { return Some((input, lhs)); };
        let (input, rhs) = Expr::parse_level(input, L)?;
        Some((input, Expr::Binary(BinExpr {
            op,
            args: [Box::new(lhs), Box::new(rhs)]
        })))
    }
}

impl BinOp {
    fn from_token(t: Token<'_>) -> Option<Self> {
        Some(match t {
            Token::Plus => Self::Plus,
            Token::Minus => Self::Minus,
            Token::Star => Self::Star,
            Token::Slash => Self::Slash,
            _ => None?
        })
    }
}

impl SinExpr {
    // SinExpr::parse -> Token::Number
    fn parse<'a>(mut input: TokenIter<'a>) -> Option<(TokenIter<'a>, Expr)> {
        let e = input.find_map(|t| if let Token::Number(n) = t {
            Some(Expr::Single(SinExpr::Number(n)))
        } else {
            None
        })?;
        Some((input, e))
    }
}

