mod prescan;

use crate::{ansi::{Color, Style}, misc::{Indexer, Interner, Ivec}, MkIndexer};

pub struct Tara {
    pub entry: ModuleId,
    interner: Interner,
    modules: Ivec<ModuleId, Module>,
    prescans: Ivec<prescan::Id, prescan::Data>,
}
impl Tara {
    pub fn from(main: &str) -> Self {
        let mut modules = Ivec::default();
        let entry = modules.push(Module::from_file(main).expect("TODO: gut error message"));
        Self {
            modules,
            entry,
            prescans: Default::default(),
            interner: Default::default(),
        }
    }
    fn push_module(&mut self, m: Module) -> ModuleId {
        self.modules.push(m)
    }
    pub fn intern(&mut self, s: &str) -> &'static str {
        self.interner.intern(s)
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
}

MkIndexer!(pub ModuleId, u32);
pub struct Module {
    source: &'static str,
    path: Box<str>,
    children: Option<(ModuleId, ModuleId)>,
}
impl Module {
    pub fn get_lexer(&self) -> crate::lexer::Lexer<'static> {
        crate::lexer::Lexer::new(self.source)
    }
    pub fn get_source(&self) -> &str {
        &self.source
    }
    fn from_file_children(path: &str) -> Vec<Self> {
        let dir: &std::path::Path = path[0..path.len() - 5].as_ref();
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
        let ppath: &std::path::Path = path.as_ref();
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
            source: Box::leak(source),
            path: Box::from(path),
            children: None,
        })
    }

    pub fn get_path(&self) -> &str {
        self.path.as_ref()
    }
}
