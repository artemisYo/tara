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

    pub const RED_PTR: Style = Style::red().merge(Style::underline());
    pub const YELLOW_PTR: Style = Style::yellow().merge(Style::underline());
    pub const ERROR: StyledStr<'static> = Style::red().apply("Error");
    pub const NOTE: StyledStr<'static> = Style::yellow().apply("Note");

    pub fn source(&self, ctx: &Tara) -> String {
        let Self::Span { module, start, .. } = *self;
        let name = ctx.get_module(module).get_path();
        format!("{}:b{}", name.to_string_lossy(), start)
    }

    pub fn line_of(
        &self,
        ctx: &Tara,
        pointer: Style,
        surround: usize,
    ) -> (usize, impl Iterator<Item = (String, String)>) {
        let Provenance::Span {
            module: span_module,
            start: span_start,
            end: span_end,
        } = *self;
        let source = ctx.get_module(span_module);
        let src = source.get_source();
        let Some(start) = src.get(..span_start) else {
            todo!();
        };
        let start = start
            .rmatch_indices("\n")
            .nth(surround)
            .map(|(n, _)| n + 1)
            .unwrap_or(0);
        let Some(end) = src.get(span_end..) else {
            todo!();
        };
        let end = end
            .match_indices("\n")
            .nth(surround)
            .map(|(n, _)| span_end + n)
            .unwrap_or(src.len());
        let text = &src[start..end];

        let digits = start.checked_ilog10().unwrap_or(0) + 1;
        let digits = digits.max(end.checked_ilog10().unwrap_or(0) + 1);
        let digits = digits as usize;
        (
            digits,
            text.lines()
                .scan(start, |c, l| {
                    let start = *c;
                    *c += l.len() + 1;
                    Some((start, l))
                })
                .map(move |(start, line)| {
                    let hl_start = span_start.saturating_sub(start).min(line.len());
                    let hl_end = span_end.saturating_sub(start).min(line.len());
                    let pretext = &line[0..hl_start];
                    let text = &line[hl_start..hl_end];
                    let posttext = &line[hl_end..];
                    (
                        format!("{}", start),
                        format!(
                            "{}{}{}",
                            Style::default().apply(pretext),
                            pointer.apply(text),
                            Style::default().apply(posttext)
                        ),
                    )
                }),
        )
    }
}

pub struct Message<'a> {
    span: Option<Provenance>,
    pointer: Style,
    kind: StyledStr<'a>,
    title: &'a str,
}
impl<'a> Message<'a> {
    pub const fn error(title: &'a str, span: Option<Provenance>) -> Self {
        Self {
            span,
            title,
            kind: Style::red().apply("Error"),
            pointer: Style::red().merge(Style::underline()),
        }
    }
    pub const fn note(title: &'a str, span: Option<Provenance>) -> Self {
        Self {
            span,
            title,
            kind: Style::yellow().apply("Note"),
            pointer: Style::yellow().merge(Style::underline()),
        }
    }
    pub fn print_header(&self, ctx: &Tara) {
        print!("[{}]", self.kind);
        if let Some(span) = self.span {
            print!("@{}", span.source(ctx));
        }
        println!(": {}", self.title);
    }
}

pub fn report_simple(kind: StyledStr, title: &str, extra: Option<&str>) {
    if let Some(extra) = extra {
        println!("╭─[{}]: {}", kind, title);
        println!("├─ {}", extra);
        println!("╰────╯");
    } else {
        println!("[{}]: {}", kind, title);
    }
}

pub fn report(ctx: &Tara, head: Message, extra: &[Message]) {
    print!("╭─");
    head.print_header(ctx);
    let mut digits = extra
        .iter()
        .filter_map(|m| Some((m.span?, m.pointer)))
        .map(|(s, p)| s.line_of(ctx, p, 1).0)
        .max()
        .unwrap_or(0);
    if let Some(span) = head.span {
        let (ds, iter) = span.line_of(ctx, head.pointer, 1);
        digits = ds.max(digits);
        for (off, line) in iter {
            println!("│ {:>digits$} │ {}", off, line);
        }
    }
    for e in extra {
        print!("├─");
        e.print_header(ctx);
        if let Some(span) = e.span {
            let (_, iter) = span.line_of(ctx, e.pointer, 1);
            for (off, line) in iter {
                println!("│ {:>digits$} │ {}", off, line);
            }
        }
    }
    print!("╰───");
    for _ in 1..digits {
        print!("─");
    }
    println!("╯");
}

fn main() {
    let mut ctx = Tara::from("example/main.tara");
    report(
        &ctx,
        Message {
            span: Some(Provenance::Span {
                module: ctx.entry,
                start: 0,
                end: ctx.get_source(ctx.entry).len(),
            }),
            pointer: Style::default(),
            kind: Style::yellow().apply("Cat"),
            title: ""
        },
        &[],
    );

    let pi = ctx.preimport(preimport::In { m: ctx.entry });
    let imports: Vec<_> = pi.imports.iter().map(|o| format!("{o:?}")).collect();
    // let ops: Vec<_> = pi.ops.iter().map(|o| format!("{o:?}")).collect();
    report(
        &ctx,
        Message {
            span: None,
            pointer: Style::default(),
            kind: Style::yellow().apply("Ops"),
            title: "",
        },
        pi.ops.iter().map(|o| Message {
            span: Some(o.loc),
            pointer: Style::underline(),
            kind: Style::yellow().apply("Operator"),
            title: "",
        }).collect::<Vec<_>>().as_ref()
    );
    report(
        &ctx,
        Message {
            span: None,
            pointer: Style::default(),
            kind: Style::yellow().apply("Imports"),
            title: "",
        },
        imports.iter().map(|i| Message {
            span: None,
            pointer: Style::default(),
            kind: Style::yellow().apply("Import"),
            title: i,
        }).collect::<Vec<_>>().as_ref()
    );

    let parse = ctx.parse(parse::In { m: ctx.entry });
    for f in &parse.ast.funcs {
        if f.name.name.0 == "main" {
            println!("{:#?}", f);
        }
    }

    let resolution = ctx.resolve(uir::In { m: ctx.entry });
    let mut main_id = None;
    for i in resolution.items.iter() {
        let uir = ctx.get_uir(*i);
        if uir.name.name.0 != "main" {
            continue;
        }
        main_id = Some(*i);
        break;
    }

    let subst = ctx.typeck(typer::In {
        i: main_id.unwrap(),
    });
    for (a, b) in subst.substitutions.iter() {
        println!("?{} := {}", a, b);
    }

    for i in resolution.items.iter() {
        ctx.typeck(typer::In {
            i: *i,
        });
    }
}
