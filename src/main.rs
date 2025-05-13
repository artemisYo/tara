mod ansi;
mod lexer;
mod misc;
mod tokens;
mod control;
mod data;
mod modules;
mod parse;
mod convert;
mod resolve;
mod typer;
mod fill;
mod codegen;

use std::io::{BufReader, Read, Seek, SeekFrom};

use data::{quir, Codegen};
use inkwell::targets::FileType;
use misc::{CharRead, Ivec};
use ansi::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Provenance {
    None,
    Span {
        module: data::files::Id,
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

    pub fn source(&self, ctx: &data::files::Files) -> Option<String> {
        let Self::Span { module, start, .. } = *self else {
            return None;
        };
        Some(format!("{}:b{}", ctx[module].path.to_string_lossy(), start))
    }

    pub fn line_of(
        &self,
        ctx: &data::files::Files,
        pointer: Style,
        surround: usize,
    ) -> Option<(usize, Vec<(String, String)>)> {
        let Provenance::Span {
            module: span_module,
            start: span_start,
            end: span_end,
        } = *self
        else {
            return None;
        };
        let source = &ctx[span_module];
        source.source.access(|src_file| {
            let src_len = source.source_len;
            let mut src = BufReader::new(src_file);

            src.seek(SeekFrom::Start(span_start.saturating_sub(1024) as u64)).unwrap();
            let start = src.take(span_start.min(1024) as u64).bytes();
            let start = start.filter_map(|i| i.ok())
                .enumerate()
                .filter(|(_, c)| *c == b'\n')
                .last()
                .map(|(i, _)| i)
                .unwrap_or(0);
            let start = start + span_start.saturating_sub(1024);

            let mut src = BufReader::new(src_file);
            src.seek(SeekFrom::Start(span_end as u64)).unwrap();
            let end = src.bytes()
                .filter_map(|i| i.ok())
                .enumerate()
                .filter(|(_, c)| *c == b'\n')
                .nth(surround)
                .map(|(i, _)| span_end + i)
                .unwrap_or(src_len);

            let mut src = BufReader::new(src_file);
            src.seek(SeekFrom::Start(start as u64)).unwrap();
            let text = CharRead::new(src.take((end - start) as u64));
            let text = String::from_iter(text);
            let text = text.lines()
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
                }).collect::<Vec<_>>();

            let digits = start.checked_ilog10().unwrap_or(0) + 1;
            let digits = digits.max(end.checked_ilog10().unwrap_or(0) + 1);
            let digits = digits as usize;

            Some((
                digits,
                text
            ))
        })
    }
}

pub struct FmtMessage<'a> {
    span: Option<Provenance>,
    pointer: Style,
    kind: StyledStr<'a>,
    title: std::fmt::Arguments<'a>,
    #[cfg(debug_assertions)]
    origin: (u32, &'static str),
}
impl<'a> FmtMessage<'a> {
    pub fn error(title: std::fmt::Arguments<'a>, origin: (u32, &'static str), span: Option<Provenance>) -> Self {
        Self {
            span,
            title,
            kind: Style::red().apply("Error"),
            pointer: Style::red().merge(Style::underline()),
            #[cfg(debug_assertions)]
            origin,
        }
    }
    pub fn note(title: std::fmt::Arguments<'a>, origin: (u32, &'static str), span: Option<Provenance>) -> Self {
        Self {
            span,
            title,
            kind: Style::yellow().apply("Note"),
            pointer: Style::yellow().merge(Style::underline()),
            #[cfg(debug_assertions)]
            origin,
        }
    }
    pub fn print_header(&self, ctx: &data::files::Files) {
        print!("[{}]", self.kind);
        if let Some(src) = self.span.and_then(|s| s.source(ctx)) {
            print!("@{}", src);
        }
        print!(": {}", self.title);

        #[cfg(debug_assertions)]
        print!(" (from {}@{})", self.origin.1, self.origin.0);

        println!();
    }
    pub fn print_header_simple(&self) {
        print!("[{}]: {}", self.kind, self.title);

        #[cfg(debug_assertions)]
        print!(" (from {}@{})", self.origin.1, self.origin.0);

        println!();
    }
}

#[macro_export]
macro_rules! message {
    (error => $fmt:literal) => {
        $crate::FmtMessage::error(format_args!($fmt), (line!(), file!()), None)
    };
    (error @ $span:expr => $fmt:literal) => {
        $crate::FmtMessage::error(format_args!($fmt), (line!(), file!()), Some($span))
    };
    (error @? $span:expr => $fmt:literal) => {
        $crate::FmtMessage::error(format_args!($fmt), (line!(), file!()), $span)
    };
    (error => $fmt:literal, $($args:tt)*) => {
        $crate::FmtMessage::error(format_args!($fmt, $($args)*), (line!(), file!()), None)
    };
    (error @ $span:expr => $fmt:literal, $($args:tt)*) => {
        $crate::FmtMessage::error(format_args!($fmt, $($args)*), (line!(), file!()), Some($span))
    };
    (error @? $span:expr => $fmt:literal, $($args:tt)*) => {
        $crate::FmtMessage::error(format_args!($fmt, $($args)*), (line!(), file!()), $span)
    };
    (note => $fmt:literal) => {
        $crate::FmtMessage::note(format_args!($fmt), (line!(), file!()), None)
    };
    (note @ $span:expr => $fmt:literal) => {
        $crate::FmtMessage::note(format_args!($fmt), (line!(), file!()), Some($span))
    };
    (note @? $span:expr => $fmt:literal) => {
        $crate::FmtMessage::note(format_args!($fmt), (line!(), file!()), $span)
    };
    (note => $fmt:literal, $($args:tt)*) => {
        $crate::FmtMessage::note(format_args!($fmt, $($args)*), (line!(), file!()), None)
    };
    (note @ $span:expr => $fmt:literal, $($args:tt)*) => {
        $crate::FmtMessage::note(format_args!($fmt, $($args)*), (line!(), file!()), Some($span))
    };
    (note @? $span:expr => $fmt:literal, $($args:tt)*) => {
        $crate::FmtMessage::note(format_args!($fmt, $($args)*), (line!(), file!()), $span)
    };
}

pub fn report_simple(title: FmtMessage, extra: &[FmtMessage]) {
    print!("╭─");
    title.print_header_simple();
    for e in extra {
        print!("├─");
        e.print_header_simple();
    }
    println!("╰───╯");
}

pub fn report(ctx: &data::files::Files, head: FmtMessage, extra: &[FmtMessage]) {
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
    let Some(root_path) = std::env::args().nth(1) else {
        report_simple(message!(error => "Please provide a path to the main file!"), &[]);
        std::process::exit(1);
    };

    let files = data::files::Files::from(root_path.into());
    files.print();
    report(
        &files,
        message!(
            note @ Provenance::Span {
                module: files.root(),
                start: 0,
                end: files[files.root()].source_len,
            } => ""
        ),
        &[],
    );

    // let mut scans = data::prescan::Scans::default();
    // let mut scanmap = prescan::Map::default();
    // let root = scanmap.query(files.root(), (&files, &mut scans));
    // scans.print(root);
    // let root = files.root();
    // files[root].source.access(|src| {
    //     let src = BufReader::new(src);
    //     for t in Tokenizer::new(root, src).step_by(5) {
    //         report(
    //             &files,
    //             message!(
    //                 note @ t.loc
    //                     => ""
    //             ),
    //             &[]
    //         );
    //     }
    // });

    let mut parsemap = parse::Map::default();
    let mut quir = convert::Data::default();
    let mut quirmap = convert::Map::default();
    // let ast = parsemap.query(modules.root(), (&modules, &mut scans, &mut scanmap, &mut imports, &mut importmap));
    // let ast = parsemap.query(files.root(), (&files,));
    // println!("{:#?}", &ast);
    let qst = quirmap.query(files.root(), (&files, &mut parsemap, &mut quir));
    // {
    //     let ast = parsemap.query(files.root(), (&files,));
    //     for f in &ast.funcs {
    //         if f.name.name == "fib" {
    //             dbg!(f);
    //         }
    //     }
    // }
    if let Some(quir::Id::Function(id)) = qst.index(&quir, "main") {
        let func = &quir.items.funcs[id];
        files.report(message!(note @ func.loc => "lol"), &[]);
    }
    // println!("{:#?}", &quir);
    let mut resmap = resolve::Map::default();
    resmap.query(qst, (&files, &quir));

    let mut tymap = typer::Map::default();
    let mut fillmap = fill::Map::default();
    let mut cgsmaps = codegen::Submaps::default();
    let mut cgmap = codegen::Map::default();
    let mut codegen = Codegen::new();
    if let Some(id) = qst.index(&quir, "_start") {
        cgmap.query(id, (&files, &mut quir, &mut cgsmaps, &mut tymap, &mut resmap, &mut fillmap, &mut codegen));
    }
    match codegen.module.verify() {
        Ok(()) => {},
        Err(msg) => {
            println!("{}", msg.to_str().unwrap());
            codegen.module.print_to_stderr();
            std::process::exit(1);
        }
    }
    codegen.target
        .write_to_file(&codegen.module, FileType::Object, "./out.o".as_ref())
        .unwrap();


    // // let resolution = ctx.lower_uir(uir::In { m: ctx.entry });
    // let items = ctx.quir(quir::In { m: ctx.entry }).namespace;
    // let _start = ctx.quir_items[items].items.get(&"_start".into()).copied();
    // // let mut _start = None;
    // // for &i in ctx.quir_items[items].items.values() {
    // //     if let uir::Item::Function(f) = &ctx.uir_items[i] {
    // //         println!(
    // //             "{}",
    // //             f.fmt(
    // //                 &ctx.uir_interfaces[i].into_func_immut(),
    // //                 &ctx.uir_locals[i],
    // //                 &ctx.uir_interfaces,
    // //                 &ctx.uir_types,
    // //                 &ctx.uir_items,
    // //             )
    // //         );
    // //     }
    // //     if ctx.item_name(i).name.0 == "_start" {
    // //         _start = Some(i);
    // //     }
    // // }

    // ctx.codegen(codegen::In { i: _start.unwrap() });
    // match ctx.llvm_mod.verify() {
    //     Ok(()) => {}
    //     Err(msg) => {
    //         println!("{}", msg.to_str().unwrap());
    //         std::process::exit(1);
    //     }
    // }

    // // Emit
    // ctx.target
    //     .write_to_file(&ctx.llvm_mod, FileType::Object, "./out.o".as_ref())
    //     .unwrap();
}
