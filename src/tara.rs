pub mod codegen;
pub mod fill;
pub mod parse;
pub mod preimport;
pub mod prescan;
pub mod typer;
pub mod uir;

use crate::{
    ansi::Style,
    misc::{Indexer, Ivec},
    report_simple, MkIndexer, Provenance,
};
use std::{
    collections::{BTreeMap as Map, BTreeSet as Set},
    path::PathBuf,
};

pub struct Tara {
    pub entry: ModuleId,
    pub modules: Ivec<ModuleId, Module>,
    pub uir_items: Ivec<uir::Id, uir::Function>,
    pub uir_types: Ivec<uir::TypeId, uir::Typekind>,
    pub llvm_mod: inkwell::module::Module<'static>,
    codegen: Map<codegen::In, Option<codegen::Out>>,
    fills: Set<fill::In>,
    typecheck: Map<typer::In, Option<typer::Out>>,
    resolution: Map<uir::In, Option<uir::Out>>,
    parses: Map<parse::In, Option<parse::Out>>,
    prescans: Map<prescan::In, Option<prescan::Out>>,
    preimports: Map<preimport::In, Option<preimport::Out>>,
}
impl Tara {
    pub fn from(main: &str) -> Self {
        let mut modules = Ivec::default();
        let top = modules.promise();
        let mut src_dir: PathBuf = main.into();
        src_dir.pop();
        modules.push(Module::top_level(src_dir, top));
        let Some(entry) = Module::from_file(main.into(), top) else {
            report_simple(
                Style::red().apply("Error"),
                &format!("Could not read file '{}'!", main),
                None,
            );
            std::process::exit(1);
        };
        let entry = modules.push(entry);
        let llvm_ctx = Box::leak(Box::new(inkwell::context::Context::create()));
        let llvm_mod = llvm_ctx.create_module("main");
        Self {
            modules,
            entry,
            llvm_mod,
            codegen: Default::default(),
            fills: Default::default(),
            uir_items: Default::default(),
            uir_types: Default::default(),
            typecheck: Default::default(),
            resolution: Default::default(),
            parses: Default::default(),
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
        let id = self
            .modules
            .push(Module::from_file(path, m).expect("too lazy to think how this'd fail"));
        self.modules[m].children.push(id);
        id
    }
    pub fn get_source(&self, m: ModuleId) -> &'static str {
        self.get_module(m).get_source()
    }
    pub fn get_lexer(&self, m: ModuleId) -> crate::lexer::Lexer<'static> {
        crate::lexer::Lexer::new(m, self.get_source(m))
    }
    pub fn get_uir(&self, u: uir::Id) -> &uir::Function {
        &self.uir_items[u]
    }
    pub fn eof_loc(&self, m: ModuleId) -> Provenance {
        let source = self.get_source(m);
        Provenance::Span {
            module: m,
            start: source.len(),
            end: source.len(),
        }
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
    pub fn get_source(&self) -> &'static str {
        self.source
    }
    pub fn get_path(&self) -> &std::path::Path {
        self.path.as_ref()
    }
    fn top_level(path: PathBuf, self_id: ModuleId) -> Self {
        Module {
            path,
            source: "",
            children: Vec::new(),
            parent: self_id,
        }
    }
    fn from_file(path: PathBuf, parent: ModuleId) -> Option<Self> {
        let ppath: &std::path::Path = path.as_ref();
        if !ppath.exists() {
            report_simple(
                Style::red().apply("Error"),
                &format!("Attempted to read nonexistent file {:?}!", &path),
                None,
            );
            std::process::exit(1);
        }

        let source = match std::fs::read_to_string(&path) {
            Ok(s) => s,
            Err(e) => {
                report_simple(
                    Style::red().apply("Error"),
                    &format!("An error occurred while trying to read file {:?}!", &path),
                    Some(&format!("Error descriptor: {}", e)),
                );
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
