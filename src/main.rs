mod ansi;
mod lexer;
mod misc;
mod tara;
mod tokens;
use inkwell::targets::FileType;
use misc::Ivec;
pub use tara::*;

use ansi::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Provenance {
    None,
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
            (Self::None, Self::None) => Self::None,
            (Self::None, e) | (e, Self::None) => *e,
        }
    }

    pub const RED_PTR: Style = Style::red().merge(Style::underline());
    pub const YELLOW_PTR: Style = Style::yellow().merge(Style::underline());
    pub const ERROR: StyledStr<'static> = Style::red().apply("Error");
    pub const NOTE: StyledStr<'static> = Style::yellow().apply("Note");

    pub fn source(&self, ctx: &Ivec<ModuleId, Module>) -> Option<String> {
        let Self::Span { module, start, .. } = *self else {
            return None;
        };
        let name = ctx[module].get_path();
        Some(format!("{}:b{}", name.to_string_lossy(), start))
    }

    pub fn line_of(
        &self,
        ctx: &Ivec<ModuleId, Module>,
        pointer: Style,
        surround: usize,
    ) -> Option<(usize, impl Iterator<Item = (String, String)>)> {
        let Provenance::Span {
            module: span_module,
            start: span_start,
            end: span_end,
        } = *self
        else {
            return None;
        };
        let source = &ctx[span_module];
        let src = source.get_source();
        let start = src.get(..span_start)?;
        let start = start
            .rmatch_indices("\n")
            .nth(surround)
            .map(|(n, _)| n + 1)
            .unwrap_or(0);
        let end = src.get(span_end..)?;
        let end = end
            .match_indices("\n")
            .nth(surround)
            .map(|(n, _)| span_end + n)
            .unwrap_or(src.len());
        let text = &src[start..end];

        let digits = start.checked_ilog10().unwrap_or(0) + 1;
        let digits = digits.max(end.checked_ilog10().unwrap_or(0) + 1);
        let digits = digits as usize;
        Some((
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
        ))
    }
}

pub struct FmtMessage<'a> {
    span: Option<Provenance>,
    pointer: Style,
    kind: StyledStr<'a>,
    title: std::fmt::Arguments<'a>,
}
impl<'a> FmtMessage<'a> {
    pub fn error(title: std::fmt::Arguments<'a>, span: Option<Provenance>) -> Self {
        Self {
            span,
            title,
            kind: Style::red().apply("Error"),
            pointer: Style::red().merge(Style::underline()),
        }
    }
    pub fn note(title: std::fmt::Arguments<'a>, span: Option<Provenance>) -> Self {
        Self {
            span,
            title,
            kind: Style::yellow().apply("Note"),
            pointer: Style::yellow().merge(Style::underline()),
        }
    }
    pub fn print_header(&self, ctx: &Ivec<ModuleId, Module>) {
        print!("[{}]", self.kind);
        if let Some(src) = self.span.and_then(|s| s.source(ctx)) {
            print!("@{}", src);
        }
        println!(": {}", self.title);
    }
}

#[macro_export]
macro_rules! message {
    (error => $fmt:literal) => {
        $crate::FmtMessage::error(format_args!($fmt), None)
    };
    (error @ $span:expr => $fmt:literal) => {
        $crate::FmtMessage::error(format_args!($fmt), Some($span))
    };
    (error @? $span:expr => $fmt:literal) => {
        $crate::FmtMessage::error(format_args!($fmt), $span)
    };
    (error => $fmt:literal, $($args:tt)*) => {
        $crate::FmtMessage::error(format_args!($fmt, $($args)*), None)
    };
    (error @ $span:expr => $fmt:literal, $($args:tt)*) => {
        $crate::FmtMessage::error(format_args!($fmt, $($args)*), Some($span))
    };
    (error @? $span:expr => $fmt:literal, $($args:tt)*) => {
        $crate::FmtMessage::error(format_args!($fmt, $($args)*), $span)
    };
    (note => $fmt:literal) => {
        $crate::FmtMessage::note(format_args!($fmt), None)
    };
    (note @ $span:expr => $fmt:literal) => {
        $crate::FmtMessage::note(format_args!($fmt), Some($span))
    };
    (note @? $span:expr => $fmt:literal) => {
        $crate::FmtMessage::note(format_args!($fmt), $span)
    };
    (note => $fmt:literal, $($args:tt)*) => {
        $crate::FmtMessage::note(format_args!($fmt, $($args)*), None)
    };
    (note @ $span:expr => $fmt:literal, $($args:tt)*) => {
        $crate::FmtMessage::note(format_args!($fmt, $($args)*), Some($span))
    };
    (note @? $span:expr => $fmt:literal, $($args:tt)*) => {
        $crate::FmtMessage::note(format_args!($fmt, $($args)*), $span)
    };
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

pub fn report(ctx: &Ivec<ModuleId, Module>, head: FmtMessage, extra: &[FmtMessage]) {
    print!("╭─");
    head.print_header(ctx);
    let mut digits = extra
        .iter()
        .filter_map(|m| Some((m.span?, m.pointer)))
        .filter_map(|(s, p)| Some(s.line_of(ctx, p, 1)?.0))
        .max()
        .unwrap_or(0);
    if let Some((ds, iter)) = head.span.and_then(|s| s.line_of(ctx, head.pointer, 1)) {
        digits = ds.max(digits);
        for (off, line) in iter {
            println!("│ {:>digits$} │ {}", off, line);
        }
    }
    for e in extra {
        print!("├─");
        e.print_header(ctx);
        if let Some((_, iter)) = e.span.and_then(|s| s.line_of(ctx, e.pointer, 1)) {
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
        &ctx.modules,
        FmtMessage {
            span: Some(Provenance::Span {
                module: ctx.entry,
                start: 0,
                end: ctx.get_source(ctx.entry).len(),
            }),
            pointer: Style::default(),
            kind: Style::yellow().apply("Cat"),
            title: format_args!(""),
        },
        &[],
    );

    let pi = ctx.preimport(preimport::In { m: ctx.entry });
    report(
        &ctx.modules,
        FmtMessage {
            span: None,
            pointer: Style::default(),
            kind: Style::yellow().apply("Ops"),
            title: format_args!(""),
        },
        pi.ops
            .iter()
            .map(|o| FmtMessage {
                span: Some(o.loc),
                pointer: Style::underline(),
                kind: Style::yellow().apply("Operator"),
                title: format_args!(""),
            })
            .collect::<Vec<_>>()
            .as_ref(),
    );

    dbg!(ctx.parse(parse::In { m: ctx.entry }).ast);

    // let resolution = ctx.lower_uir(uir::In { m: ctx.entry });
    let items = ctx.quir(quir::In { m: ctx.entry }).namespace;
    let _start = ctx.quir_items[items].items.get(&"_start".into()).copied();
    // let mut _start = None;
    // for &i in ctx.quir_items[items].items.values() {
    //     if let uir::Item::Function(f) = &ctx.uir_items[i] {
    //         println!(
    //             "{}",
    //             f.fmt(
    //                 &ctx.uir_interfaces[i].into_func_immut(),
    //                 &ctx.uir_locals[i],
    //                 &ctx.uir_interfaces,
    //                 &ctx.uir_types,
    //                 &ctx.uir_items,
    //             )
    //         );
    //     }
    //     if ctx.item_name(i).name.0 == "_start" {
    //         _start = Some(i);
    //     }
    // }

    ctx.codegen(codegen::In { i: _start.unwrap() });
    match ctx.llvm_mod.verify() {
        Ok(()) => {}
        Err(msg) => {
            println!("{}", msg.to_str().unwrap());
            std::process::exit(1);
        }
    }

    // Emit
    ctx.target
        .write_to_file(&ctx.llvm_mod, FileType::Object, "./out.o".as_ref())
        .unwrap();
}
