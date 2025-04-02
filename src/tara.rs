pub mod codegen;
pub mod fill;
pub mod parse;
pub mod preimport;
pub mod prescan;
pub mod typer;
// pub mod uir;
pub mod quir;
pub mod resolve;

use inkwell::targets::{self, TargetMachine};

use crate::{ansi::Style, misc::Ivec, report_simple, MkIndexer, Provenance};
use crate::{report, FmtMessage};
use std::{
    collections::BTreeMap as Map,
    path::PathBuf,
};

#[macro_export]
macro_rules! gen_query {
    ($name:ident) => {
        impl $crate::tara::Tara {
            pub fn $name(&mut self, i: In) -> Out {
                match self.$name.get(&i) {
                    Some(Some(o)) => o.cheap(),
                    Some(None) => panic!(
                        "{} entered a cycle!\nWith input: {:?}",
                        stringify!($name), i
                    ),
                    None => {
                        self.$name.insert(i, None);
                        let data = $name(self, i);
                        self.$name.insert(i, Some(data));
                        self.$name.get(&i).unwrap().cheap().unwrap()
                    },
                }
            }
        }
    };
}

pub struct Tara {
    // stores
    pub entry: ModuleId,
    pub modules: Ivec<ModuleId, Module>,

    // pub uir_items: Ivec<uir::Id, uir::Item>,
    // pub uir_interfaces: Ivec<uir::Id, uir::Interface>,
    // pub uir_locals: Ivec<uir::Id, uir::LocalVec>,
    // pub uir_types: Ivec<uir::TypeId, uir::Typekind>,
    // uir_tvars: usize,

    pub quir_items: Ivec<quir::Id, quir::Item>,
    pub quir_types: Ivec<quir::TypeId, quir::Typekind>,
    pub quir_interns: Map<quir::Typekind, quir::TypeId>,
    quir_tvars: usize,

    pub llvm_mod: inkwell::module::Module<'static>,
    pub target: TargetMachine,
    // passes
    codegen: Map<codegen::In, Option<codegen::Out>>,
    fill: Map<fill::In, Option<fill::Out>>,
    typecheck: Map<typer::In, Option<typer::Out>>,
    quir: Map<quir::In, Option<quir::Out>>,
    // lower_uir: Map<uir::In, Option<uir::Out>>,
    resolve: Map<resolve::In, Option<resolve::Out>>,
    parse: Map<parse::In, Option<parse::Out>>,
    prescan: Map<prescan::In, Option<prescan::Out>>,
    preimport: Map<preimport::In, Option<preimport::Out>>,
}
// impl<I: Into<uir::Id>> std::ops::Index<(I, uir::ExprId)> for Tara {
//     type Output = uir::Expr;
//     fn index(&self, (i, e): (I, uir::ExprId)) -> &Self::Output {
//         &self.uir_locals[i.into()][e]
//     }
// }
// impl<I: Into<uir::Id>> std::ops::Index<(I, uir::BindingId)> for Tara {
//     type Output = uir::Binding;
//     fn index(&self, (i, e): (I, uir::BindingId)) -> &Self::Output {
//         &self.uir_locals[i.into()][e]
//     }
// }

impl Tara {
    #[inline]
    pub fn report(&self, head: FmtMessage, extra: &[FmtMessage]) {
        report(&self.modules, head, extra);
    }
    // #[inline]
    // pub fn item_name<I: Into<uir::Id>>(&self, i: I) -> parse::Ident {
    //     self.uir_interfaces[i.into()].name()
    // }
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
        targets::Target::initialize_native(&targets::InitializationConfig {
            asm_parser: true,
            asm_printer: true,
            base: true,
            disassembler: false,
            info: false,
            machine_code: true,
        })
        .unwrap();
        let triple = TargetMachine::get_default_triple();
        let target = targets::Target::from_triple(&triple).unwrap();
        let cpu = TargetMachine::get_host_cpu_name();
        let features = TargetMachine::get_host_cpu_features();
        let target = target
            .create_target_machine(
                &triple,
                cpu.to_str().unwrap(),
                features.to_str().unwrap(),
                inkwell::OptimizationLevel::None,
                targets::RelocMode::Default,
                targets::CodeModel::Default,
            )
            .unwrap();
        Self {
            modules,
            entry,
            llvm_mod,
            target,
            codegen: Default::default(),
            fill: Default::default(),
            // uir_items: Default::default(),
            // uir_types: Default::default(),
            // uir_locals: Default::default(),
            // uir_interfaces: Default::default(),
            // uir_tvars: 0,
            quir_items: Default::default(),
            quir_types: Default::default(),
            quir_interns: Default::default(),
            quir_tvars: 0,
            typecheck: Default::default(),
            // lower_uir: Default::default(),
            parse: Default::default(),
            prescan: Default::default(),
            preimport: Default::default(),
            quir: Default::default(),
            resolve: Default::default(),
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
    // pub fn get_uir(&self, u: uir::Id) -> &uir::Item {
    //     &self.uir_items[u]
    // }
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
