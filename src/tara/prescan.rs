use std::num::NonZero;
use std::{fmt::Write, rc::Rc};

use crate::misc::Istr;
use crate::{
    ansi::Style,
    tokens::{Token, Tokenkind},
    Module, ModuleId, Provenance, Tara,
};

#[derive(Debug, Clone, Copy)]
pub struct Opdef {
    lbp: Option<NonZero<u32>>,
    rbp: Option<NonZero<u32>>,
    spelling: Istr,
}

#[derive(Clone)]
pub struct Out {
    // this vec is cloned in preimport
    // it's bad, but reasonable as it needs to be extended
    pub ops: Vec<Opdef>,
    pub tokens: Rc<[Token<Istr>]>,
    pub imports: Rc<[(Provenance, Box<[(Provenance, Istr)]>)]>,
}
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct In {
    pub m: ModuleId,
}

impl Tara {
    pub fn prescan(&mut self, i: In) -> Out {
        match self.prescans.get(&i) {
            Some(o) => o.clone(),
            None => {
                let data = prescan(self, i);
                self.prescans.insert(i, data);
                self.prescans.get(&i).unwrap().clone()
            }
        }
    }
}

fn prescan(ctx: &mut Tara, i: In) -> Out {
    use Tokenkind::*;
    let m = i.m;
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
                let mut path = vec![(part.loc, part.text.into())];
                while lexer
                    .peek()
                    .filter(|t| t.kind == Slash)
                    .map(|_| ())
                    .and_then(|_| lexer.next())
                    .is_some()
                {
                    let Some(part) = expect(ctx, m, &mut lexer, &[Name, Ellipsis]) else {
                        break;
                    };
                    path.push((part.loc, part.text.into()));
                    if part.kind == Ellipsis {
                        break;
                    }
                }
                imports.push((t.loc, path.into_boxed_slice()));
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
                let text = cat.as_str().into();
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
                let name = name.text.into();
                let lbp = lhs.text.parse::<u32>().map(|n| n + 1).unwrap_or(0);
                let rbp = rhs.text.parse::<u32>().map(|n| n + 1).unwrap_or(0);
                ops.push(Opdef {
                    lbp: NonZero::new(lbp),
                    rbp: NonZero::new(rbp),
                    spelling: name,
                });
            }

            _ => {
                let text = t.text.into();
                tokens.push(Token {
                    kind: t.kind,
                    loc: t.loc,
                    text,
                });
            }
        }
    }
    Out {
        ops,
        tokens: tokens.into(),
        imports: imports.into(),
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

fn expect<'a>(
    ctx: &Tara,
    m: ModuleId,
    lexer: &mut std::iter::Peekable<impl Iterator<Item = Token<&'a str>>>,
    kinds: &[Tokenkind],
) -> Option<Token<&'a str>> {
    if let Some(t) = lexer.next_if(|t| kinds.contains(&t.kind)) {
        Some(t)
    } else {
        let m = ctx.get_module(m);
        match lexer.peek() {
            Some(t) => unexpected_token(m, t.loc, t.kind.spelling(), kinds),
            None => unexpected_token(m, m.eof_loc(), "EOF", kinds),
        }
        None
    }
}

fn unexpected_token<S: AsRef<str>>(source: &Module, loc: Provenance, sp: S, exps: &[Tokenkind]) {
    let title = if exps.len() == 1 {
        format!(
            "Expected '{}', but got '{}'!",
            exps[0].spelling(),
            sp.as_ref(),
        )
    } else {
        let mut title = String::from("Expected one of ");
        for e in &exps[0..] {
            _ = write!(title, "'{}', ", e.spelling());
        }
        _ = write!(title, "but got '{}'!", sp.as_ref());
        title
    };
    loc.report(
        source,
        Style::red() | Style::underline(),
        Style::red().apply("Error"),
        &title,
        [].into_iter(),
    );
}
