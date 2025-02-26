#![allow(dead_code)]
// ^ only temporary, as the code is getting developed
// should be removed soon

mod lexer;
mod misc;
mod prescan;
mod tokens;

use std::path::Path;

use misc::{Ansi, Ivec, Indexer, Istr};
use tokens::Token;
use prescan::{Opdef, OpsId};

#[derive(Debug, Clone, Copy)]
pub struct Provenance {
    pub start: usize,
    pub end: usize,
}

impl Provenance {
    pub fn report(&self, source: &str) {
		let Some(start) = source.get(..self.start) else { return; };
        let start = start.rfind("\n")
			.map(|n| n + 1)
            .unwrap_or(0);
		let Some(end) = source.get(self.start..) else { return; };
		let end = end.find("\n")
            .map(|n| self.start + n)
            .unwrap_or(source.len());
        let pretext = &source[start..self.start];
        let text = &source[self.start..self.end.min(end)];
        let posttext = &source[self.end.min(end)..end];
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

// basically view types as per niko matsakis
struct ModuleAccess<'tara> {
	pub entry: ModuleId,
	modules: &'tara mut Ivec<ModuleId, Module>,
}
impl ModuleAccess<'_> {
	pub fn get_source(&self, m: ModuleId) -> &str {
		let module = &self.modules[m];
		module.source.as_ref()
	}
	pub fn get_lexer(&self, m: ModuleId) -> impl Iterator<Item = Token<'_>> {
		let source = self.get_source(m);
		lexer::Lexer::new(source)
	}
}

struct InternAccess<'tara> {
	interner: &'tara mut misc::Interner,
}
impl InternAccess<'_> {
	pub fn intern(&mut self, s: &str) -> Istr {
		self.interner.intern(s)
	}
}

struct Tara {
    pub entry: ModuleId,
	interner: misc::Interner,
    modules: Ivec<ModuleId, Module>,
}

impl Tara {
	pub fn split_for_prescan<'tara>(&'tara mut self) -> (ModuleAccess<'tara>, InternAccess<'tara>) {
		let entry = self.entry;
		let modules = &mut self.modules;
		let interner = &mut self.interner;
		(
			ModuleAccess { entry, modules },
			InternAccess { interner }
		)
	}
	pub fn intern(&mut self, s: &str) -> Istr {
		self.interner.intern(s)
	}
    pub fn from(main: &str) -> Self {
        let mut modules = Ivec::default();
        let entry = modules.push(Module::from_file(main).expect("TODO: gut error message"));
        Self { modules, entry, interner: Default::default() }
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
	pub fn get_lexer<'a>(&'a self, m: ModuleId) -> impl Iterator<Item = Token<'a>> {
		let source = self.get_source(m);
		lexer::Lexer::new(source)
	}
	pub fn place_ops(&mut self, m: ModuleId, ops: Box<[Opdef]>) {
		assert!(self.get_module(m).ops.is_none());
		self.modules[m].ops = Some(ops);
	}
	pub fn place_tokens(&mut self, m: ModuleId, tokens: Box<[Token<'static>]>) {
		assert!(self.get_module(m).tokens.is_none());
		self.modules[m].tokens = Some(tokens);
	}
	pub fn set_ops(&mut self, m: ModuleId) -> OpsId {
		if self.get_module(m).ops.is_some() {
			return OpsId::assume(m);
		}
		prescan::prescan(self, m)
	}
	pub fn get_ops(&self, m: OpsId) -> &[Opdef] {
		self.get_module(m.module())
			.ops
			.as_ref()
			.expect("OpsId should signal that prescan was already run")
	}
}

MkIndexer!(ModuleId, u32);
struct Module {
    source: Box<str>,
    path: Box<str>,
    children: Option<(ModuleId, ModuleId)>,
	ops: Option<Box<[Opdef]>>,
	tokens: Option<Box<[Token<'static>]>>
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
			ops: None,
			tokens: None,
        })
    }
}

fn main() {
    let mut ctx = Tara::from("examples/main.tara");
    println!("[cat]:");
    println!("{}", ctx.get_source(ctx.entry));

    println!("[lex]:");
	for t in ctx.get_lexer(ctx.entry) {
        println!("{:?}", t);
    }

	println!("[rep]:");
	for t in ctx.get_lexer(ctx.entry) {
        t.loc.report(ctx.get_source(ctx.entry));
    }
	
	println!("[ops]:");
	let ops = ctx.set_ops(ctx.entry);
	for o in ctx.get_ops(ops) {
		println!("{:?}", o);
	}
}
