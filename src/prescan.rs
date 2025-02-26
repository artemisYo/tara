use std::num::NonZero;

use crate::{
    tokens::{Token, Tokenkind},
    Module, ModuleId, Query, Tara,
};

#[derive(Debug, Clone, Copy)]
pub struct Opdef {
    lbp: Option<NonZero<u32>>,
    rbp: Option<NonZero<u32>>,
    spelling: &'static str,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct OpsId(ModuleId);

pub struct Prescan<'tara> {
    module: &'tara mut Module,
    interner: &'tara mut crate::misc::Interner,
}
impl Query for Prescan<'_> {
    type Input = ModuleId;
    type Id = OpsId;
    type Data<'a> = Prescan<'a>;
    type Output = (
        Box<[Token<'static>]>,
        Box<[Opdef]>,
        Box<[Vec<&'static str>]>,
    );

    fn get(tara: &Tara, i: Self::Id) -> &Self::Output {
        tara.modules[i.0]
            .prescan
            .as_ref()
            .expect("Self::Id indicates the value exists")
    }

    fn check(tara: &mut Tara, m: ModuleId) -> Result<OpsId, Prescan> {
        if tara.modules[m].prescan.is_some() {
            Ok(OpsId(m))
        } else {
            Err(Prescan {
                module: &mut tara.modules[m],
                interner: &mut tara.interner,
            })
        }
    }

    fn run(data: Prescan) -> Self::Output {
        use Tokenkind::*;
        let mut ops = Vec::new();
        let mut tokens = Vec::new();
        let mut imports = Vec::new();
        let lexer = data.module.get_lexer();
        let lexer = lexer.filter(|t| t.kind != Comment);
        let mut lexer = lexer.peekable();
        while let Some(t) = lexer.next() {
            match t.kind {
                Import => {
                    let Some(part) = lexer.next_if(|t| t.kind == Name) else {
                        todo!("Report error: expected @k, but got @t at @loc");
                    };
                    let mut path = vec![data.interner.intern(part.text)];
                    while lexer
                        .peek()
                        .filter(|t| t.kind == Slash)
                        .map(|_| ())
                        .and_then(|_| lexer.next())
                        .is_some()
                    {
                        let Some(part) = lexer.next_if(|t| t.kind == Name || t.kind == Ellipsis)
                        else {
                            todo!("Report error: expected @k, but got @t at @loc");
                        };
                        path.push(data.interner.intern(part.text));
                        if part.kind == Ellipsis {
                            break;
                        }
                    }
                    imports.push(path);
                    lexer
                        .next_if(|t| t.kind == Semicolon)
                        .or_else(|| todo!("Report error: expected @k, but got @t at @loc"));
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
                    let text = data.interner.intern(cat.as_ref());
                    tokens.push(Token {
                        kind: String,
                        text,
                        loc,
                    });
                }

                Operator => {
                    lexer
                        .next_if(|t| t.kind == OpenParen)
                        .or_else(|| todo!("Report error: expected @k, but got @t at @loc"));
                    let Some(lhs) = lexer.next_if(|t| t.kind == Number || t.kind == Underscore)
                    else {
                        todo!("Report error: expected @k, but got @t at @loc");
                    };
                    lexer
                        .next_if(|t| t.kind == Comma)
                        .or_else(|| todo!("Report error: expected @k, but got @t at @loc"));
                    let Some(name) = lexer.next_if(|t| t.kind == Name) else {
                        todo!("Report error: expected @k, but got @t at @loc");
                    };
                    lexer
                        .next_if(|t| t.kind == Comma)
                        .or_else(|| todo!("Report error: expected @k, but got @t at @loc"));
                    let Some(rhs) = lexer.next_if(|t| t.kind == Number || t.kind == Underscore)
                    else {
                        todo!("Report error: expected @k, but got @t at @loc");
                    };
                    lexer
                        .next_if(|t| t.kind == CloseParen)
                        .or_else(|| todo!("Report error: expected @k, but got @t at @loc"));
                    lexer
                        .next_if(|t| t.kind == Semicolon)
                        .or_else(|| todo!("Report error: expected @k, but got @t at @loc"));
                    let name = data.interner.intern(name.text);
                    let lbp = lhs.text.parse::<u32>().map(|n| n + 1).unwrap_or(0);
                    let rbp = rhs.text.parse::<u32>().map(|n| n + 1).unwrap_or(0);
                    ops.push(Opdef {
                        lbp: NonZero::new(lbp),
                        rbp: NonZero::new(rbp),
                        spelling: name,
                    });
                }

                _ => {
                    let text = data.interner.intern(t.text);
                    tokens.push(Token {
                        kind: t.kind,
                        loc: t.loc,
                        text,
                    });
                }
            }
        }
        drop(lexer);
        (
            tokens.into_boxed_slice(),
            ops.into_boxed_slice(),
            imports.into_boxed_slice(),
        )
    }

    fn finish(tara: &mut crate::Tara, m: ModuleId, data: Self::Output) -> Self::Id {
        assert!(tara.modules[m].prescan.replace(data).is_none());
        OpsId(m)
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
