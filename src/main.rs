mod ansi;
mod lexer;
mod misc;
mod tara;
mod tokens;
use inkwell::{
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine}, OptimizationLevel,
};
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
    pub fn print_header(&self, ctx: &Ivec<ModuleId, Module>) {
        print!("[{}]", self.kind);
        if let Some(src) = self.span.and_then(|s| s.source(ctx)) {
            print!("@{}", src);
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

pub fn report(ctx: &Ivec<ModuleId, Module>, head: Message, extra: &[Message]) {
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
        Message {
            span: Some(Provenance::Span {
                module: ctx.entry,
                start: 0,
                end: ctx.get_source(ctx.entry).len(),
            }),
            pointer: Style::default(),
            kind: Style::yellow().apply("Cat"),
            title: "",
        },
        &[],
    );

    let pi = ctx.preimport(preimport::In { m: ctx.entry });
    let imports: Vec<_> = pi.imports.iter().map(|o| format!("{o:?}")).collect();
    // let ops: Vec<_> = pi.ops.iter().map(|o| format!("{o:?}")).collect();
    report(
        &ctx.modules,
        Message {
            span: None,
            pointer: Style::default(),
            kind: Style::yellow().apply("Ops"),
            title: "",
        },
        pi.ops
            .iter()
            .map(|o| Message {
                span: Some(o.loc),
                pointer: Style::underline(),
                kind: Style::yellow().apply("Operator"),
                title: "",
            })
            .collect::<Vec<_>>()
            .as_ref(),
    );
    report(
        &ctx.modules,
        Message {
            span: None,
            pointer: Style::default(),
            kind: Style::yellow().apply("Imports"),
            title: "",
        },
        imports
            .iter()
            .map(|i| Message {
                span: None,
                pointer: Style::default(),
                kind: Style::yellow().apply("Import"),
                title: i,
            })
            .collect::<Vec<_>>()
            .as_ref(),
    );

    // let parse = ctx.parse(parse::In { m: ctx.entry });
    // for f in &parse.ast.funcs {
    //     if f.name.name.0 == "main" {
    //         println!("{:#?}", f);
    //     }
    // }

    let resolution = ctx.resolve(uir::In { m: ctx.entry });
    let mut main_id = None;
    let mut _start = None;
    for i in resolution.items.iter() {
        let uir = ctx.get_uir(*i);
        if uir.name.name.0 == "_start" {
            _start = Some(*i);
        }
        if uir.name.name.0 == "main" {
            main_id = Some(*i);
        }
    }

    {
        let main_fmt = ctx.uir_items[main_id.unwrap()].fmt(&ctx.uir_types, &ctx.uir_items);
        println!("{}\n", main_fmt);
    }

    let subst = ctx.typeck(typer::In {
        i: main_id.unwrap(),
    });
    for (a, b) in subst.substitutions.iter() {
        println!("?{} := {}", a, ctx.uir_types[*b].fmt(&ctx.uir_types));
    }

    {
        let main_fmt = ctx.uir_items[main_id.unwrap()].fmt(&ctx.uir_types, &ctx.uir_items);
        println!("{}\n", main_fmt);
    }

    ctx.fill(fill::In {
        i: main_id.unwrap(),
    });

    {
        let main_fmt = ctx.uir_items[main_id.unwrap()].fmt(&ctx.uir_types, &ctx.uir_items);
        println!("{}\n", main_fmt);
    }

    for &i in resolution.items.iter() {
        ctx.fill(fill::In { i });
    }

    // ctx.codegen(codegen::In { i: main_id.unwrap() });
    ctx.codegen(codegen::In { i: _start.unwrap() });
    ctx.llvm_mod.print_to_stderr();
    match ctx.llvm_mod.verify() {
        Ok(()) => {}
        Err(msg) => {
            println!("{}", msg.to_str().unwrap());
            std::process::exit(1);
        }
    }

    // Emit
    Target::initialize_native(&InitializationConfig {
        asm_parser: true,
        asm_printer: true,
        base: true,
        disassembler: false,
        info: false,
        machine_code: true,
    })
    .unwrap();
    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple).unwrap();
    let cpu = TargetMachine::get_host_cpu_name();
    let features = TargetMachine::get_host_cpu_features();
    let machine = target
        .create_target_machine(
            &triple,
            cpu.to_str().unwrap(),
            features.to_str().unwrap(),
            OptimizationLevel::None,
            RelocMode::Default,
            CodeModel::Default,
        )
        .unwrap();
    machine
        .write_to_file(&ctx.llvm_mod, FileType::Object, "./out.o".as_ref())
        .unwrap();
}
