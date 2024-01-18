use super::*;
use crate::lexer::{Token, TokenIter};

pub fn parse<'a>(input: &'a [Token<'a>]) -> Option<Root> {
    File::parse(TokenIter::new(input)).map(|(_, e)| e)
}

type Parser<T> = for<'a> fn(TokenIter<'a>) -> Option<(TokenIter<'a>, T)>;

impl File {
    // File::parse -> Statement* Expr?
    fn parse(mut input: TokenIter) -> Option<(TokenIter, Self)> {
        let mut body = vec![];
        while let Some((rest, stmt)) = Statement::parse(input) {
            body.push(stmt);
            input = rest;
        }
        let tail = if let Some((rest, tail)) = Expr::parse(input) {
            input = rest;
            Some(tail)
        } else {
            None
        };
        Some((input, Self { body, tail, typing: None }))
    }
}

impl Statement {
    fn parse(input: TokenIter) -> Option<(TokenIter, Self)> {
        const STMTS: &[Parser<Statement>] = &[LetStmt::parse, Statement::parse_expr];
        STMTS
            .iter()
            .map(|f| f(input))
            .find(Option::is_some)
            .flatten()
    }
    fn parse_expr(input: TokenIter) -> Option<(TokenIter, Self)> {
        let (mut input, e) = Expr::parse(input)?;
        input.consume(Token::Semicolon)?;
        Some((input, Statement::Expr(e)))
    }
}

impl LetStmt {
    // LetStmt::parse -> "let" Token::Name "=" Expr ";"
    fn parse(mut input: TokenIter) -> Option<(TokenIter, Statement)> {
        input.consume(Token::Let)?;
        let name = input.find_map(|t| {
            if let Token::Name(n) = t {
                Some(n.to_string())
            } else {
                None
            }
        })?;
        input.consume(Token::Equals)?;
        let (mut input, init) = Expr::parse(input)?;
        input.consume(Token::Semicolon)?;
        Some((input, Statement::Let(LetStmt { name, init, typing: None })))
    }
}

impl Expr {
    // Expr::parse -> Expr::parse_level(0)
    fn parse<'a>(input: TokenIter<'a>) -> Option<(TokenIter<'a>, Self)> {
        Self::parse_level(input, 0)
    }
    // Expr::parse_level(p) -> EXPRS[p]
    fn parse_level<'a>(input: TokenIter<'a>, level: usize) -> Option<(TokenIter<'a>, Self)> {
        const EXPRS: &[&[Parser<Expr>]] = &[
            &[WhileExpr::parse, IfExpr::parse, BinExpr::parse::<0>],
            &[BinExpr::parse::<1>],
            &[Expr::parse_parens, SinExpr::parse],
        ];
        EXPRS[level]
            .iter()
            .map(|f| f(input))
            .find(Option::is_some)
            .flatten()
    }
    // Expr::parse_parens -> "(" Expr::parse ")"
    fn parse_parens(mut input: TokenIter) -> Option<(TokenIter, Self)> {
        input.consume(Token::OpenParen)?;
        let (mut input, e) = Self::parse(input)?;
        input.consume(Token::CloseParen)?;
        Some((input, e))
    }
}

impl Block {
    // Block::parse -> "{" File "}"
    fn parse(mut input: TokenIter) -> Option<(TokenIter, Self)> {
        input.consume(Token::OpenCurly)?;
        let (mut input, f) = File::parse(input)?;
        input.consume(Token::CloseCurly)?;
        Some((input, Self(Box::new(f))))
    }
}

impl IfExpr {
    // IfExpr::parse -> "if" Expr IfExpr::block { "else" IfExpr::block }?
    fn parse(mut input: TokenIter) -> Option<(TokenIter, Expr)> {
        input.consume(Token::If)?;
        let (input, cond) = Expr::parse(input)?;
        let cond = Box::new(cond);
        let (mut input, smash) = Block::parse(input)?;
        let pass = if input.consume(Token::Else).is_some() {
            let (rest, pass) = Block::parse(input)?;
            input = rest;
            Some(pass)
        } else {
            None
        };
        Some((input, Expr::If(Self { cond, smash, pass, typing: None })))
    }
}

impl WhileExpr {
    // WhileExpr::parse -> "while" Expr IfExpr::block { "then" IfExpr::block }?
    fn parse(mut input: TokenIter) -> Option<(TokenIter, Expr)> {
        input.consume(Token::While)?;
        let (input, cond) = Expr::parse(input)?;
        let cond = Box::new(cond);
        let (mut input, body) = Block::parse(input)?;
        let then = if input.consume(Token::Then).is_some() {
            let (rest, then) = Block::parse(input)?;
            input = rest;
            Some(then)
        } else {
            None
        };
        Some((input, Expr::While(Self { cond, body, then, typing: None })))
    }
}

impl BinExpr {
    // BinExpr::parse<L> -> Expr::parse_level(L + 1) { BINOP[L] Expr::parse_level(L) }?
    fn parse<'a, const L: usize>(input: TokenIter<'a>) -> Option<(TokenIter<'a>, Expr)> {
        const BINOP: &[&[Token]] = &[&[Token::Plus, Token::Minus], &[Token::Star, Token::Slash]];
        let (mut input, lhs) = Expr::parse_level(input, L + 1)?;
        let Some(op) = BINOP[L]
            .iter()
            .find(|t| input.consume(**t).is_some())
            .map(|t| BinOp::from_token(*t).unwrap())
        else {
            return Some((input, lhs));
        };
        let (input, rhs) = Expr::parse_level(input, L)?;
        Some((
            input,
            Expr::Binary(BinExpr {
                op,
                args: [Box::new(lhs), Box::new(rhs)],
                typing: None,
            }),
        ))
    }
}

impl BinOp {
    fn from_token(t: Token<'_>) -> Option<Self> {
        Some(match t {
            Token::Plus => Self::Plus,
            Token::Minus => Self::Minus,
            Token::Star => Self::Star,
            Token::Slash => Self::Slash,
            _ => None?,
        })
    }
}

impl SinExpr {
    // SinExpr::parse -> Token::Number / Token::Name
    fn parse<'a>(mut input: TokenIter<'a>) -> Option<(TokenIter<'a>, Expr)> {
        let e = input.find_map(|t| match t {
            Token::Number(n) => Some(Expr::Single(SinExpr::Number(n))),
            Token::Name(n) => Some(Expr::Single(SinExpr::Name(n.to_string()))),
            _ => None,
        })?;
        Some((input, e))
    }
}
