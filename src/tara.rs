pub mod preimport;
pub mod prescan;

use crate::{
    ansi::Style,
    misc::{Indexer, Ivec},
    MkIndexer, Provenance,
};
use std::{collections::BTreeMap as Map, path::PathBuf};

pub struct Tara {
    pub entry: ModuleId,
    modules: Ivec<ModuleId, Module>,
    prescans: Map<prescan::In, prescan::Out>,
    preimports: Map<preimport::In, preimport::Out>,
}
impl Tara {
    pub fn from(main: &str) -> Self {
        let mut modules = Ivec::default();
        let entry = modules.promise();
        modules.push(Module::from_file(main.into(), entry).expect("TODO: gut error message"));
        Self {
            modules,
            entry,
            prescans: Default::default(),
            preimports: Default::default(),
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
        for c in &m.children {
            let c = &self.modules[*c];
            self.print_submodule(c, level + 1);
        }
    }
    pub fn get_module(&self, m: ModuleId) -> &Module {
        &self.modules[m]
    }
    pub fn get_sibling(&mut self, m: ModuleId, s: &str) -> ModuleId {
        let m = self.modules[m].parent;
        self.get_child(m, s)
    }
    pub fn get_child(&mut self, m: ModuleId, s: &str) -> ModuleId {
        // probably not gut
        for &c in &self.modules[m].children {
            if self.modules[c].path.file_name().unwrap().to_str().unwrap() == s {
                return c;
            }
        }
        let mut path = self.modules[m].path.clone();
        path.set_extension("");
        path.push(s);
        path.set_extension("tara");
        let id = self.modules.push(Module::from_file(path, m).expect("too lazy to think how this'd fail"));
        self.modules[m].children.push(id);
        id
    }
    pub fn get_source(&self, m: ModuleId) -> &str {
        self.get_module(m).get_source()
    }
}

MkIndexer!(pub ModuleId, u32);
pub struct Module {
    path: PathBuf,
    source: &'static str,
    children: Vec<ModuleId>,
    parent: ModuleId,
}
impl Module {
    pub fn eof_loc(&self) -> Provenance {
        Provenance {
            start: self.source.len(),
            end: self.source.len(),
        }
    }
    pub fn get_lexer(&self) -> crate::lexer::Lexer<'static> {
        crate::lexer::Lexer::new(self.source)
    }
    pub fn get_source(&self) -> &str {
        &self.source
    }
    pub fn get_path(&self) -> &std::path::Path {
        self.path.as_ref()
    }
    fn from_file(path: PathBuf, parent: ModuleId) -> Option<Self> {
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

        let source = match std::fs::read_to_string(&path) {
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

        Some(Self {
            path,
            source: String::leak(source),
            children: Vec::new(),
            parent,
        })
    }
}
