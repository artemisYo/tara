#![allow(dead_code)]
// ^ only temporary, as the code is getting developed
// should be removed soon

mod lexer;
mod misc;
mod prescan;
mod tokens;

use std::path::Path;

use misc::{Ansi, IVec, Indexer};

#[derive(Clone, Copy)]
pub struct Provenance<'s> {
    pub start: usize,
    pub end: usize,
    pub source: &'s str,
}
impl std::fmt::Debug for Provenance<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Provenance(bytes {}:{})", self.start, self.end)
    }
}

impl Provenance<'_> {
    pub fn report(&self) {
        let start = self.source[..self.start]
            .rfind("\n")
            .map(|n| n + 1)
            .unwrap_or(0);
        let end = self.source[self.start..]
            .find("\n")
            .map(|n| self.start + n)
            .unwrap_or(self.source.len());
        let pretext = &self.source[start..self.start];
        let text = &self.source[self.start..self.end.min(end)];
        let posttext = &self.source[self.end.min(end)..end];
        println!("╭─[rprt]: at bytes [{}:{}]", self.start, self.end);
        println!(
            "│ {}{}{}{}{}{}",
            pretext,
            misc::Ansi::Red,
            misc::Ansi::Underline,
            text,
            misc::Ansi::Default,
            posttext
        );
        println!("╰───");
    }
}

struct Tara {
    pub entry: ModuleId,
    modules: IVec<ModuleId, Module>,
}
impl Tara {
    pub fn from(main: &str) -> Self {
        let mut modules = IVec::default();
        let entry = modules.push(Module::from_file(main).expect("TODO: gut error message"));
        Self { modules, entry }
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
    pub fn modules_from_files<'a>(
        &mut self,
        paths: impl Iterator<Item = &'a str>,
    ) -> (ModuleId, ModuleId) {
        self.modules.push_range(paths.filter_map(Module::from_file))
    }
}

MkIndexer!(ModuleId, u32);
struct Module {
    source: Box<str>,
    path: Box<str>,
    children: Option<(ModuleId, ModuleId)>,
}
impl Module {
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
                    Ansi::Red,
                    Ansi::Default,
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
                Ansi::Red,
                Ansi::Default,
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
                    Ansi::Red,
                    Ansi::Default,
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
        })
    }
}

fn main() {
    let ctx = Tara::from("examples/main.tara");
    let main_src = ctx.get_module(ctx.entry).get_source();
    println!("[cat]:");
    println!("{}", main_src);

    println!("[lex]:");
    for t in lexer::Lexer::from(main_src) {
        println!("{:?}", t);
    }

    for t in lexer::LexerIter::new(main_src) {
        t.loc.report();
    }
}
