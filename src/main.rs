mod ansi;
mod lexer;
mod misc;
mod prescan;
mod preimport;
mod tokens;

use std::path::Path;

use ansi::*;
use misc::{Indexer, Ivec};

#[derive(Debug, Clone, Copy)]
pub struct Provenance {
    pub start: usize,
    pub end: usize,
}

impl Provenance {
    pub fn meet(&self, other: &Self) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
    pub fn report(
        &self,
        source: &Module,
        pointer: Style,
        kind: StyledStr,
        title: &str,
        notes: &[&str],
    ) {
		let src = source.get_source();
		let name = source.path.as_ref();
        let Some(start) = src.get(..self.start) else {
            return;
        };
        let start = start.rfind("\n").map(|n| n + 1).unwrap_or(0);
        let Some(end) = src.get(self.end..) else {
            return;
        };
        let end = end.find("\n").map(|n| self.end + n).unwrap_or(src.len());
        let text = &src[start..end];

        let digits = start.checked_ilog10().unwrap_or(0) + 1;
        let digits = digits.max(end.checked_ilog10().unwrap_or(0) + 1);
        let digits = digits as usize;
        println!("╭─[{}]@{}:b{}: {}", kind, name, self.start, title);
        for (start, line) in text.lines().scan(start, |c, l| {
            let start = *c;
            *c += l.len() + 1;
            Some((start, l))
        }) {
            let hl_start = self.start.saturating_sub(start);
            let hl_end = self.end.saturating_sub(start).min(line.len());
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
        if !notes.is_empty() {
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
        println!("╯");
    }
}

pub trait Query: Sized {
    type Input: Copy;
    type Output;
    type Data<'a>;
    type Id;

    fn get(_: &Tara, _: Self::Id) -> &Self::Output;
    fn check(_: &mut Tara, _: Self::Input) -> Result<Self::Id, Self::Data<'_>>;
    fn run(_: Self::Data<'_>) -> Self::Output;
    // stores the query result, preferably without changing `Output`
    fn finish(_: &mut Tara, _: Self::Input, _: Self::Output) -> Self::Id;
}

pub struct Tara {
    pub entry: ModuleId,
    interner: misc::Interner,
    modules: Ivec<ModuleId, Module>,
}
impl Tara {
    pub fn from(main: &str) -> Self {
        let mut modules = Ivec::default();
        let entry = modules.push(Module::from_file(main).expect("TODO: gut error message"));
        Self {
            modules,
            entry,
            interner: Default::default(),
        }
    }
    pub fn print_modules(&self) {
        self.print_submodule(&self.modules[self.entry], 0);
    }
    fn print_submodule(&self, m: &Module, level: usize) {
        for _ in 0..level {
            print!("  ");
        }
        println!("| {:?}", &m.path);
        if let Some(cs) = m.children {
            for c in &self.modules[cs] {
                self.print_submodule(c, level + 1);
            }
        }
    }
    pub fn get_module(&self, m: ModuleId) -> &Module {
        &self.modules[m]
    }
    pub fn get_children(&mut self, m: ModuleId) -> (ModuleId, ModuleId) {
        if let Some(cs) = self.modules[m].children {
            return cs;
        }
        let children = Module::from_file_children(&self.modules[m].path);
        let cs = self.modules.push_range(children.into_iter());
        self.modules[m].children = Some(cs);
        self.get_children(m)
    }
    pub fn get_source(&self, m: ModuleId) -> &str {
        self.get_module(m).get_source()
    }
    pub fn get_query<Q: Query>(&mut self, i: Q::Input) -> Q::Id {
        Q::check(self, i)
            .map_err(|e| Q::run(e))
            .unwrap_or_else(|o| Q::finish(self, i, o))
    }
    pub fn query<Q: Query>(&self, i: Q::Id) -> &Q::Output {
        Q::get(self, i)
    }
}

MkIndexer!(pub ModuleId, u32);
pub struct Module {
    source: Box<str>,
    path: Box<str>,
    children: Option<(ModuleId, ModuleId)>,
    prescan: Option<<prescan::Prescan<'static> as Query>::Output>,
}
impl Module {
    pub fn get_lexer(&self) -> lexer::Lexer {
        lexer::Lexer::new(&self.source)
    }
    pub fn get_source(&self) -> &str {
        &self.source
    }
    fn from_file_children(path: &str) -> Vec<Self> {
        let dir: &Path = path[0..path.len() - 5].as_ref();
        if !dir.is_dir() {
            return Vec::new();
        }
        let dir = match dir.read_dir() {
            Ok(d) => d,
            Err(e) => {
                println!(
                    "[{}Error{}]: Module directory {:?} exists, but is not readable!",
                    Color::Red,
                    Style::default(),
                    dir
                );
                println!("| {}", e);
                std::process::exit(1);
            }
        };
        let mut out = Vec::new();
        for e in dir.filter_map(Result::ok) {
            let p = e.path();
            let Some(s) = p.to_str() else {
                continue;
            };
            let Some(m) = Module::from_file(s) else {
                continue;
            };
            out.push(m);
        }
        out
    }
    fn from_file(path: &str) -> Option<Self> {
        let ppath: &Path = path.as_ref();
        if !ppath.exists() {
            println!(
                "[{}Error{}]: Attempted to read nonexistent file {:?}!",
                Style::red(),
                Style::default(),
                &path
            );
            std::process::exit(1);
        }
        if !ppath.is_file() {
            return None;
        }
        if ppath.extension()? != "tara" {
            return None;
        }

        let source = match std::fs::read_to_string(path) {
            Ok(s) => s,
            Err(e) => {
                println!(
                    "[{}Error{}]: An error occurred while trying to read file {:?}!",
                    Style::red(),
                    Style::default(),
                    &path
                );
                println!("| {}", e);
                std::process::exit(1);
            }
        };
        let source = source.into_boxed_str();

        Some(Self {
            source,
            path: Box::from(path),
            children: None,
            prescan: None,
        })
    }
}

fn main() {
    let mut ctx = Tara::from("example/main.tara");
    Provenance {
        start: 0,
        end: ctx.get_source(ctx.entry).len(),
    }
    .report(
        ctx.get_module(ctx.entry),
        Style::default(),
        Style::yellow().apply("Cat"),
        "",
        &[],
    );
    // println!("{}", ctx.get_source(ctx.entry));

    // println!("[lex]:");
    // for t in ctx.get_module(ctx.entry).get_lexer() {
    //     println!("{:?}", t);
    // }

    let _ops = ctx.get_query::<prescan::Prescan>(ctx.entry);
    // println!("[scan]:");
    // for t in &ctx.query::<prescan::Prescan>(ops).0 {
    //     println!("{:?}", t);
    // }

    // println!("[scan rep]:");
    // for t in &ctx.query::<prescan::Prescan>(ops).0 {
    //     t.loc.report(
    //         ctx.get_source(ctx.entry),
    //         Style::cyan().apply("Report"),
    //         "current token is:",
    //         &[],
    //     );
    // }

    // println!("[ops]:");
    // for o in &ctx.query::<prescan::Prescan>(ops).1 {
    //     println!("{:?}", o);
    // }

    // println!("[imports]:");
    // for o in &ctx.query::<prescan::Prescan>(ops).2 {
    //     println!("{:?}", o);
    // }
}
