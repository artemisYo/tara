mod ansi;
mod lexer;
mod misc;
mod tara;
mod tokens;
pub use tara::*;

use ansi::*;

#[derive(Debug, Clone, Copy)]
pub enum Provenance {
    Span {
        module: ModuleId,
        start: usize,
        end: usize,
    },
}

impl Provenance {
    pub fn meet(&self, other: &Self) -> Self {
        match (self, other) {
            (
                Self::Span {
                    module: m1,
                    start: s1,
                    end: e1,
                },
                Self::Span {
                    module: m2,
                    start: s2,
                    end: e2,
                },
            ) => {
                // sanity check; should not actually become a problem
                assert_eq!(m1, m2, "cannot merge cross-module locations");
                Self::Span {
                    module: *m1,
                    start: *s1.min(s2),
                    end: *e1.max(e2),
                }
            }
        }
    }
    pub fn report<'a>(
        &self,
        ctx: &Tara,
        pointer: Style,
        kind: StyledStr,
        title: &str,
        notes: impl Iterator<Item = &'a str>,
    ) {
        let Provenance::Span {
            module: span_module,
            start: span_start,
            end: span_end
        } = *self;
        let source = ctx.get_module(span_module);
        let mut notes = notes.peekable();
        let notes_empty = notes.peek().is_none();
        let src = source.get_source();
        let name = source.get_path();
        let Some(start) = src.get(..span_start) else {
            return;
        };
        let start = start.rfind("\n").map(|n| n + 1).unwrap_or(0);
        let Some(end) = src.get(span_end..) else {
            return;
        };
        let end = end.find("\n").map(|n| span_end + n).unwrap_or(src.len());
        let text = &src[start..end];

        let digits = start.checked_ilog10().unwrap_or(0) + 1;
        let digits = digits.max(end.checked_ilog10().unwrap_or(0) + 1);
        let digits = digits as usize;
        println!(
            "╭─[{}]@{}:b{}: {}",
            kind,
            name.to_string_lossy(),
            span_start,
            title
        );
        for (start, line) in text.lines().scan(start, |c, l| {
            let start = *c;
            *c += l.len() + 1;
            Some((start, l))
        }) {
            let hl_start = span_start.saturating_sub(start);
            let hl_end = span_end.saturating_sub(start).min(line.len());
            let pretext = &line[0..hl_start];
            let text = &line[hl_start..hl_end];
            let posttext = &line[hl_end..];
            println!(
                "│ {:digits$} │ {}{}{}",
                start,
                Style::default().apply(pretext),
                pointer.apply(text),
                Style::default().apply(posttext)
            );
        }
        if !notes_empty {
            println!("├─[{}Notes{}]:", Style::yellow(), Style::default());
            for n in notes {
                let mut lines = n.lines();
                let Some(first) = lines.next() else {
                    continue;
                };
                println!("│ - {}", first);
                for l in lines {
                    println!("│   {}", l);
                }
            }
        }
        print!("╰──");
        for _ in 0..digits {
            print!("─");
        }
        println!("{}", if notes_empty { "╯" } else { "─" });
    }
}

fn main() {
    let mut ctx = Tara::from("example/main.tara");
    Provenance::Span {
        module: ctx.entry,
        start: 0,
        end: ctx.get_source(ctx.entry).len(),
    }
    .report(
        &ctx,
        Style::default(),
        Style::yellow().apply("Cat"),
        "",
        [].into_iter(),
    );

    let pi = ctx.preimport(preimport::In { m: ctx.entry });
    let imports: Vec<_> = pi.imports.iter().map(|o| format!("{o:?}")).collect();
    let ops: Vec<_> = pi.ops.iter().map(|o| format!("{o:?}")).collect();
    Provenance::Span {
        module: ctx.entry,
        start: 0,
        end: 0,
    }
    .report(
        &ctx,
        Style::default(),
        Style::yellow().apply("Ops"),
        "",
        ops.iter().map(|s| s.as_ref()),
    );
    Provenance::Span {
        module: ctx.entry,
        start: 0,
        end: 0,
    }
    .report(
        &ctx,
        Style::default(),
        Style::yellow().apply("Ops"),
        "",
        imports.iter().map(|s| s.as_ref()),
    );

    let parse = ctx.parse(parse::In { m: ctx.entry });
    for f in &parse.ast.funcs {
        f.loc.report(
            &ctx,
            Style::default(),
            Style::yellow().apply("Func"),
            "This is a print out of a function",
            [].into_iter(),
        );
        f.args.loc().report(
            &ctx,
            Style::underline(),
            Style::yellow().apply("Binding"),
            "These are the functions args",
            [].into_iter(),
        );
        f.ret.loc.report(
            &ctx,
            Style::underline(),
            Style::yellow().apply("Type"),
            "This is the functions return type",
            [].into_iter(),
        );
        f.body.loc.report(
            &ctx,
            Style::underline(),
            Style::yellow().apply("Expr"),
            "This is the functions body",
            [].into_iter(),
        );
    }

    let resolution = ctx.resolve(uir::In { m: ctx.entry });
    let mut main_id = None;
    for i in resolution.items.iter() {
        let uir = ctx.get_uir(*i);
        if uir.name.0 != "main" {
            continue;
        }
        main_id = Some(*i);
        println!("Locals of uir item 'main':\n{:#?}", uir.locals);
        break;
    }

    let subst = ctx.typeck(typer::In {i: main_id.unwrap()});
    println!("{:#?}", subst.substitutions);
}
