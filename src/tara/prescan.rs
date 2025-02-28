use std::fmt::Write;
use std::num::NonZero;

use crate::{
    ansi::Style, tokens::{Token, Tokenkind}, MkIndexer, misc::Indexer, Module, ModuleId, Tara
};

#[derive(Debug, Clone, Copy)]
pub struct Opdef {
    lbp: Option<NonZero<u32>>,
    rbp: Option<NonZero<u32>>,
    spelling: &'static str,
}

impl Tara {
    pub fn prescan(&mut self, m: ModuleId) -> Id {
        // .find is not gut efficient
        match self.prescans.find(|d| d.m == m) {
            Some(i) => i,
            None => {
                let data = prescan(self, m);
                self.prescans.push(data)
            }
        }
    }
}

MkIndexer!(pub Id, u32);
pub struct Data {
    m: ModuleId,
    ops: Vec<Opdef>,
    tokens: Box<[Token<'static>]>,
    imports: Box<[Box<[&'static str]>]>
}

fn prescan(ctx: &mut Tara, m: ModuleId) -> Data {
    use Tokenkind::*;
    let mut ops = Vec::new();
    let mut tokens = Vec::new();
    let mut imports = Vec::new();
    let lexer = ctx.get_module(m).get_lexer();
    let lexer = lexer.filter(|t| t.kind != Comment);
    let mut lexer = lexer.peekable();
    'outer: while let Some(t) = lexer.next() {
        match t.kind {
            Import => {
                let Some(part) = expect(ctx, m, &mut lexer, &[Name]) else {
                    continue 'outer;
                };
                let mut path = vec![ctx.intern(part.text)];
                while lexer
                    .peek()
                    .filter(|t| t.kind == Slash)
                    .map(|_| ())
                    .and_then(|_| lexer.next())
                    .is_some()
                {
                    let Some(part) = expect(ctx, m, &mut lexer, &[Name, Ellipsis]) else {
                        continue 'outer;
                    };
                    path.push(ctx.intern(part.text));
                    if part.kind == Ellipsis {
                        break;
                    }
                }
                imports.push(path.into_boxed_slice());
                if expect(ctx, m, &mut lexer, &[Semicolon]).is_none() {
                    continue 'outer;
                }
            }

            String => {
                let mut cat = std::string::String::new();
                let mut loc = t.loc;
                cat_str(&mut cat, t.text);
                while let Some(t) = lexer
                    .peek()
                    .filter(|t| t.kind == String)
                    .map(|_| ())
                    .and_then(|_| lexer.next())
                {
                    loc = loc.meet(&t.loc);
                    cat_str(&mut cat, t.text);
                }
                let text = ctx.intern(cat.as_ref());
                tokens.push(Token {
                    kind: String,
                    text,
                    loc,
                });
            }

            Operator => {
                if expect(ctx, m, &mut lexer, &[OpenParen]).is_none() {
                    continue 'outer;
                }
                let Some(lhs) = expect(ctx, m, &mut lexer, &[Number, Underscore]) else {
                    continue 'outer;
                };
                if expect(ctx, m, &mut lexer, &[Comma]).is_none() {
                    continue 'outer;
                }
                let Some(name) = expect(ctx, m, &mut lexer, &[Name]) else {
                    continue 'outer;
                };
                if expect(ctx, m, &mut lexer, &[Comma]).is_none() {
                    continue 'outer;
                }
                let Some(rhs) = expect(ctx, m, &mut lexer, &[Number, Underscore]) else {
                    continue 'outer;
                };
                if expect(ctx, m, &mut lexer, &[CloseParen]).is_none() {
                    continue 'outer;
                }
                if expect(ctx, m, &mut lexer, &[Semicolon]).is_none() {
                    continue 'outer;
                }
                let name = ctx.intern(name.text);
                let lbp = lhs.text.parse::<u32>().map(|n| n + 1).unwrap_or(0);
                let rbp = rhs.text.parse::<u32>().map(|n| n + 1).unwrap_or(0);
                ops.push(Opdef {
                    lbp: NonZero::new(lbp),
                    rbp: NonZero::new(rbp),
                    spelling: name,
                });
            }

            _ => {
                let text = ctx.intern(t.text);
                tokens.push(Token {
                    kind: t.kind,
                    loc: t.loc,
                    text,
                });
            }
        }
    }
    Data {
        m,
        ops,
        tokens: tokens.into_boxed_slice(),
        imports: imports.into_boxed_slice()
    }
}

fn expect<'a>(
    ctx: &Tara,
    m: ModuleId,
    lexer: &mut std::iter::Peekable<impl Iterator<Item = Token<'a>>>,
    kinds: &[Tokenkind],
) -> Option<Token<'a>> {
    if let Some(t) = lexer.next_if(|t| kinds.contains(&t.kind)) {
        return Some(t);
    } else {
        if let Some(t) = lexer.peek() {
            unexpected_token(ctx.get_module(m), *t, kinds);
        }
        return None;
    }
}

fn cat_str(b: &mut String, s: &str) {
    let s = s
        .strip_prefix('"')
        .expect("should always be there in string literals");
    b.reserve(s.len());
    let iter = s.chars();
    let iter = iter.scan(false, |state, elem| {
        let current = *state;
        *state = elem == '\\' && !*state;
        Some((current, elem))
    });
    for c in iter {
        match c {
            (false, '"') => break,
            (false, '\\') => {}
            (false, '\n') => {
                b.push('\n');
                break;
            }
            (true, '\n') => break,
            (true, '0') => b.push('\0'),
            (true, 'n') => b.push('\n'),
            (true, 't') => b.push('\t'),
            (_, c) => b.push(c),
        }
    }
}

fn unexpected_token(source: &Module, t: Token, exps: &[Tokenkind]) {
    let title = if exps.len() == 1 {
        format!(
            "Expected '{}', but got '{}'!",
            exps[0].spelling(),
                t.kind.spelling()
        )
    } else {
        let mut title = String::from("Expected one of ");
        for e in &exps[0..] {
            _ = write!(title, "'{}', ", e.spelling());
        }
        _ = write!(title, "but got '{}'!", t.kind.spelling());
        title
    };
    t.loc.report(
        source,
        Style::red() | Style::underline(),
                 Style::red().apply("Error"),
                 &title,
                 &[],
    );
}
