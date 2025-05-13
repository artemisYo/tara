use inkwell::targets::{self, TargetMachine};

use crate::Provenance;

macro_rules! MkSimpleEnum {
    (
        $(# $attr:tt)?
        $vis:vis enum $name:ident {
            $($case:ident),*
            $(,)?
        }
    ) => {
        $(#$attr)?
        #[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
        $vis enum $name {
            $($case),*
        }
        impl $name {
            pub const VALUES: &'static [$name] = {
                use $name::*;
                &[
                    $($case),*
                ]
            };
        }
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ident {
    pub name: &'static str,
    pub loc: Provenance,
}

pub mod files {
    use crate::misc::GuardedFile;
    use crate::Ivec;
    use crate::MkIndexer;

    MkIndexer!(pub Id, u32);
    #[derive(Debug)]
    pub struct File {
        pub path: std::path::PathBuf,
        pub source: GuardedFile,
        pub source_len: usize,
        pub children: Vec<Id>,
    }
    #[derive(Debug)]
    pub struct Files(pub Ivec<Id, File>);
}

pub mod ast {
    use std::num::NonZero;
    use crate::Provenance;
    use super::Ident;


    #[derive(Debug, Clone, Copy)]
    pub struct Op {
        pub lbp: Option<NonZero<u32>>,
        pub rbp: Option<NonZero<u32>>,
        pub spelling: &'static str,
    }

    #[derive(Debug)]
    pub struct Ast {
        pub funcs: Vec<Function>,
        pub types: Vec<Typedecl>,
        pub imports: Vec<Import>,
    }

    #[derive(Debug, Clone)]
    pub struct Import {
        pub loc: Provenance,
        pub path: Vec<Ident>,
    }

    #[derive(Debug)]
    pub struct Function {
        pub loc: Provenance,
        pub name: Ident,
        pub ret: Type,
        pub args: Binding,
        pub body: Expr,
    }

    #[derive(Debug)]
    pub struct Typedecl {
        pub loc: Provenance,
        pub name: Ident,
        pub cases: Vec<Typecase>,
    }

    #[derive(Debug)]
    pub struct Typecase {
        pub loc: Provenance,
        pub name: Ident,
        pub binding: Binding,
    }

    #[derive(Debug, Clone)]
    pub struct Type {
        pub loc: Provenance,
        pub kind: Typekind,
    }
    #[derive(Debug, Clone)]
    pub enum Typekind {
        Func { args: Box<Type>, ret: Box<Type> },
        Call { args: Box<Type>, func: Box<Type> },
        Bundle(Vec<Type>),
        Recall(Vec<Ident>),
    }

    #[derive(Debug)]
    pub struct Binding {
        pub loc: Provenance,
        pub kind: binding::Kind,
    }
    pub mod binding {
        use crate::{data::{files, quir, Ident, Quir}, misc::Ivec, CommonEnum, Provenance};
        use super::{Binding, Type};

        CommonEnum! {
            #[derive(Debug)]
            pub enum Kind {
                Empty: Empty,
                Name: Name,
                Tuple: Tuple,
                Constructor: Box<Constructor>,
            }

            pub &self.convert(
                module: files::Id,
                loc: Provenance,
                mutable: bool,
                type_hole: fn(&mut Quir) -> quir::types::Id,
                data: &mut Quir,
                bindings: &mut Ivec<quir::binding::Id, quir::Binding>
            ) -> quir::binding::Id;
        }

        #[derive(Debug)]
        pub struct Empty;
        #[derive(Debug)]
        pub struct Name(pub &'static str, pub Option<Type>);
        #[derive(Debug)]
        pub struct Tuple(pub Vec<Binding>);
        #[derive(Debug)]
        pub struct Constructor {
            pub constructor_loc: Provenance,
            pub constructor: Vec<Ident>,
            pub fields: Binding,
        }
    }

    #[derive(Debug, Clone)]
    pub struct Expr {
        pub loc: Provenance,
        pub kind: expr::Kind,
    }
    pub mod expr {
        use crate::{data::{files, quir, Ident, Quir}, CommonEnum, Provenance};

        use super::{Binding, Expr};

        CommonEnum! {
            #[derive(Debug, Clone)]
            pub enum Kind {
                If: std::rc::Rc<If>,
                Call: std::rc::Rc<Call>,
                Tuple: std::rc::Rc<Tuple>,
                Loop: std::rc::Rc<Loop>,
                Bareblock: std::rc::Rc<Bareblock>,
                Path: std::rc::Rc<Path>,
                Number: std::rc::Rc<Number>,
                String: std::rc::Rc<String>,
                Bool: std::rc::Rc<Bool>,
                Let: std::rc::Rc<Let>,
                Mut: std::rc::Rc<Mut>,
                Assign: std::rc::Rc<Assign>,
                Break: std::rc::Rc<Break>,
                Return: std::rc::Rc<Return>,
                Const: std::rc::Rc<Const>,
            }

            pub &self.convert(
                module: files::Id,
                loc: Provenance,
                data: &mut Quir,
                locals: &mut quir::Locals
            ) -> quir::expr::Id;
        }

        #[derive(Debug)]
        pub struct If {
            pub cond: Expr,
            pub smash: Expr,
            pub pass: Expr,
        }

        #[derive(Debug)]
        pub struct Call {
            pub func: Expr,
            pub args: Expr,
        }

        #[derive(Debug)]
        pub struct Tuple(pub Vec<Expr>);
        #[derive(Debug)]
        pub struct Loop(pub Expr);
        #[derive(Debug)]
        pub struct Bareblock(pub Vec<Expr>);

        #[derive(Debug)]
        pub struct Path(pub Vec<Ident>);
        #[derive(Debug)]
        pub struct Number(pub &'static str);
        #[derive(Debug)]
        pub struct String(pub &'static str);
        #[derive(Debug)]
        pub struct Bool(pub &'static str);

        #[derive(Debug)]
        pub struct Let(pub Binding, pub Expr);
        #[derive(Debug)]
        pub struct Mut(pub Binding, pub Expr);
        #[derive(Debug)]
        pub struct Assign(pub Expr, pub Expr);
        #[derive(Debug)]
        pub struct Break(pub Option<Expr>);
        #[derive(Debug)]
        pub struct Return(pub Option<Expr>);
        #[derive(Debug)]
        pub struct Const(pub Expr);
    }
}

#[derive(Default, Debug)]
pub struct Quir {
    pub items: quir::Items,
    pub types: quir::Types,
}
pub mod quir {
    use std::{cell::OnceCell, collections::BTreeMap};

    use super::Ident;
    use crate::{misc::Ivec, MkIndexer, Provenance};

    // top level
    MkIndexer!(pub TypecaseId, u32);
    MkIndexer!(pub TypedeclId, u32);
    MkIndexer!(pub FunctionId, u32);
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub enum Id {
        Typecase(TypecaseId),
        Typedecl(TypedeclId),
        Function(FunctionId),
    }
    #[derive(Default, Debug)]
    pub struct Items {
        pub funcs: Ivec<FunctionId, Function>,
        pub typedecls: Ivec<TypedeclId, Typedecl>,
        pub typecases: Ivec<TypecaseId, Typecase>,
    }

   
    #[derive(Debug, Clone)]
    pub struct Import {
        pub loc: Provenance,
        pub path: Vec<Ident>,
        pub target: OnceCell<Id>,
    }

    #[derive(Debug)]
    pub struct Typedecl {
        pub loc: Provenance,
        pub name: Ident,
        pub imports: Vec<Import>,
        pub cases: Vec<TypecaseId>,
        pub items: BTreeMap<&'static str, Id>,
        pub inherits: bool,
    }
    #[derive(Debug)]
    pub struct Typecase {
        pub loc: Provenance,
        pub name: Ident,
        pub binding: binding::Id,
        pub parent: TypedeclId,
        pub index: usize,
        // since we never actually have exprs
        pub bindings: Ivec<binding::Id, Binding>,
    }
    #[derive(Debug)]
    pub struct Function {
        pub loc: Provenance,
        pub name: Ident,
        // pub typ: Type,
        pub ret: Type,
        pub binding: Type,
        pub body: expr::Id,
        pub locals: Locals,
    }

    // expr level
    #[derive(Debug, Clone, Copy)]
    pub enum LocalId {
        Expr(expr::Id),
        Binding(binding::Id),
    }
    #[derive(Default, Debug)]
    pub struct Locals {
        pub expr: Ivec<expr::Id, Expr>,
        pub bindings: Ivec<binding::Id, Binding>,
    }

    #[derive(Debug)]
    pub struct Binding {
        pub loc: Provenance,
        pub typ: Type,
        pub kind: binding::Kind,
    }
    pub mod binding {
        use std::cell::OnceCell;

        use super::{Binding, FunctionId, Items, TypecaseId, Types};
        use crate::{codegen, data::{files::Files, Ident, Quir}, misc::Ivec, resolve, typer, CommonEnum, MkIndexer, Provenance};
        MkIndexer!(pub Id, u32);

        CommonEnum! {
            #[derive(Debug)]
            pub enum Kind {
                Empty: Empty,
                Name: Name,
                Tuple: Tuple,
                Constructor: Constructor,
            }

            pub &self.resolve(
                files: &Files,
                ctx: &resolve::Ctx,
                quir: &Quir,
                binds: &Ivec<Id, Binding>
            ) -> ();

            pub &self.register(
                files: &Files,
                ctx: &mut resolve::Ctx,
                quir: &Quir,
                binds: &Ivec<Id, Binding>,
                id: Id
            ) -> ();

            pub &self.fill(
                files: &Files,
                substs: &typer::Data,
                items: &Items,
                types: &mut Types,
                owner: FunctionId
            ) -> ();

            pub &self.check(
                mapbd: &mut codegen::Mapbd,
                inputs: codegen::Inputsbd<'_>
            ) -> codegen::Result;
        }

        #[derive(Debug, Clone, Copy)]
        pub struct Empty;
        #[derive(Debug, Clone, Copy)]
        pub struct Name(pub &'static str, pub bool);
        #[derive(Debug)]
        pub struct Tuple(pub Vec<Id>);
        #[derive(Debug)]
        pub struct Constructor {
            pub loc: Provenance,
            pub id: OnceCell<TypecaseId>,
            pub constructor: Vec<Ident>,
            pub fields: Id,
        }
    }

    #[derive(Debug)]
    pub struct Expr {
        pub loc: Provenance,
        pub typ: Type,
        pub kind: expr::Kind,
    }
    pub mod expr {
        use std::cell::OnceCell;

        use either::Either;
        use crate::{codegen, data::{files::Files, Ident, Quir}, resolve, typer, CommonEnum, MkIndexer, Provenance};
        use super::{binding, FunctionId, Items, Locals, Type, Types};

        MkIndexer!(pub Id, u32);
        CommonEnum! {
            #[derive(Debug)]
            pub enum Kind {
                If: If,
                Call: Call,
                Builtin: Builtinkind,
                Tuple: Tuple,
                Loop: Loop,
                Bareblock: Bareblock,
                Recall: Recall,
                Number: Number,
                String: String,
                Bool: Bool,
                Arguments: Arguments,
                Poison: Poison,
                Let: Let,
                Assign: Assign,
                Break: Break,
                Return: Return,
                Const: Const,
            }

            pub &self.resolve(
                files: &Files,
                ctx: &mut resolve::Ctx,
                quir: &Quir,
                locals: &Locals,
                loc: Provenance,
                id: Id
            ) -> ();

            pub &self.typeck(
                ctx: &mut typer::Ctx,
                loc: Provenance,
                typ: Type
            ) -> typer::Result;

            pub &self.fill(
                files: &Files,
                substs: &typer::Data,
                items: &Items,
                types: &mut Types,
                owner: FunctionId
            ) -> ();

            pub &self.codegen(
                map: &mut codegen::Mapxp,
                ty: Type,
                inputs: codegen::Inputsxp<'_>
            ) -> codegen::Result;
        }

        #[derive(Debug)]
        pub struct If {
            pub cond: Id,
            pub smash: Id,
            pub pass: Id,
        }
        #[derive(Debug)]
        pub struct Call {
            pub func: Id,
            pub args: Id,
        }

        MkSimpleEnum! {
            pub enum Builtinkind {
                Add,
                Sub,
                Mul,
                Div,
                Mod,
                And,
                Or,
                Xor,
                ShLeft,
                ShRight,
                Not,
                Negate,
                CmpEq,
                CmpNE,
                CmpGt,
                CmpLt,
                CmpGE,
                CmpLE,
                Syscall,
                PtrToInt,
                IntToPtr
            }
        }
        impl Builtinkind {
            pub const fn spelling(self) -> &'static str {
                use Builtinkind::*;
                match self {
                    Add => "__builtin_add",
                    Sub => "__builtin_sub",
                    Mul => "__builtin_mul",
                    Div => "__builtin_div",
                    Mod => "__builtin_mod",
                    And => "__builtin_and",
                    Or => "__builtin_or",
                    Xor => "__builtin_xor",
                    ShLeft => "__builtin_shl",
                    ShRight => "__builtin_shr",
                    Not => "__builtin_not",
                    Negate => "__builtin_negate",
                    CmpEq => "__builtin_cmp_eq",
                    CmpNE => "__builtin_cmp_ne",
                    CmpGt => "__builtin_cmp_gt",
                    CmpLt => "__builtin_cmp_lt",
                    CmpGE => "__builtin_cmp_ge",
                    CmpLE => "__builtin_cmp_le",
                    Syscall => "__builtin_syscall",
                    PtrToInt => "__builtin_ptr_to_int",
                    IntToPtr => "__builtin_int_to_ptr",
                }
            }
        }

        #[derive(Debug)]
        pub struct Tuple(pub Vec<Id>);
        #[derive(Debug)]
        pub struct Loop(pub Id);
        #[derive(Debug)]
        pub struct Bareblock(pub Vec<Id>);
        #[derive(Debug)]
        pub struct Recall(pub Vec<Ident>, pub OnceCell<Either<binding::Id, super::Id>>);
        #[derive(Debug)]
        pub struct Number(pub &'static str);
        #[derive(Debug)]
        pub struct String(pub &'static str);
        #[derive(Debug)]
        pub struct Bool(pub &'static str);
        #[derive(Debug)]
        pub struct Arguments;
        #[derive(Debug)]
        pub struct Poison;

        #[derive(Debug)]
        pub struct Let(pub binding::Id, pub Id);
        #[derive(Debug)]
        pub struct Assign(pub Id, pub Id);
        #[derive(Debug)]
        pub struct Break {
            pub val: Id,
            pub target: OnceCell<Id>,
        }
        #[derive(Debug)]
        pub struct Return(pub Id);
        #[derive(Debug)]
        pub struct Const(pub Id);
    }

    // type level
    #[derive(Debug, Default)]
    pub struct Types {
        pub types: Ivec<types::Id, types::Kind>,
        pub typemap: BTreeMap<types::Kind, types::Id>,
        pub tvar_count: usize,
    }
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub struct Type {
        pub loc: Provenance,
        pub kind: types::Id,
    }
    pub mod types {
        use std::cell::OnceCell;

        use crate::{data::{files::{self, Files}, Ident, Quir}, resolve, CommonEnum, MkIndexer, Provenance};
        use super::TypedeclId;
        MkIndexer!(pub Id, u32);

        CommonEnum! {
            #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
            pub enum Kind {
                Func: Func,
                Call: Call,
                Bundle: Bundle,
                Recall: Recall,
                Var: Var,
                String: String,
                Bool: Bool,
                Int: Int,
                Tup: Tup,
            }

            pub &self.resolve(
                files: &Files,
                ctx: &resolve::Ctx,
                quir: &Quir,
                loc: Provenance
            ) -> ();
        }

        #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
        pub struct Func {
            pub args: Id,
            pub ret: Id,
        }
        #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
        pub struct Call {
            pub args: Id,
            pub func: Id,
        }
        #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
        pub struct Bundle(pub Vec<Id>);
        // we tag recall with ModuleId,
        // as interning types may deduplicate
        // a recall across modules, causing later
        // name resolution to modify recalls with
        // the same name, but different contexts
        #[derive(Clone, Debug)]
        pub struct Recall(pub Vec<Ident>, pub files::Id, pub OnceCell<TypedeclId>);
        impl Eq for Recall {}
        impl PartialEq for Recall {
            fn eq(&self, other: &Self) -> bool {
                self.0.eq(&other.0) && self.1.eq(&other.1)
            }
        }
        impl PartialOrd for Recall {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }
        impl Ord for Recall {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                self.0.cmp(&other.0).then(self.1.cmp(&other.1))
            }
        }
        #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
        pub struct Var(pub usize);
        #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
        pub struct String;
        #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
        pub struct Bool;
        #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
        pub struct Int;
        #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
        pub struct Tup;
    }
    pub struct TypeFmt<'a> {
        pub root: types::Id,
        pub loc: Option<Provenance>,
        pub types: &'a Ivec<types::Id, types::Kind>,
    }
    impl Type {
        pub fn fmt<'a>(&self, types: &'a Ivec<types::Id, types::Kind>) -> TypeFmt<'a> {
            TypeFmt {
                root: self.kind,
                loc: Some(self.loc),
                types
            }
        }
    }
    impl TypeFmt<'_> {
        fn sub(&self, new_root: types::Id) -> Self {
            TypeFmt {
                root: new_root,
                loc: None,
                types: self.types,
            }
        }
    }
    impl std::fmt::Debug for TypeFmt<'_> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self)
        }
    }
    impl std::fmt::Display for TypeFmt<'_> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            use types::Kind::*;
            if let Some(loc) = self.loc {
                write!(f, "(@{:?}) ", loc)?;
            }
            match &self.types[self.root] {
                Func(types::Func { args, ret }) =>
                    write!(f, "func({}): {}", self.sub(*args), self.sub(*ret)),
                Call(types::Call { args, func }) =>
                    write!(f, "{}{}", self.sub(*func), self.sub(*args)),
                Bundle(types::Bundle(ref ids)) => {
                    write!(f, "(")?;
                    if !ids.is_empty() {
                        write!(f, "({}", self.sub(ids[0]))?;
                        for &i in &ids[1..] {
                            write!(f, ", {}", self.sub(i))?;
                        }
                    }
                    write!(f, ")")
                },
                Recall(types::Recall(path, _, _)) => {
                    write!(f, "{}", path[0].name)?;
                    for &n in &path[1..] {
                        write!(f, "/{}", n.name)?;
                    }
                    write!(f, "")
                }
                Var(types::Var(v)) => write!(f, "?{}", v),
                String(_) => write!(f, "string"),
                Bool(_) => write!(f, "bool"),
                Int(_) => write!(f, "int"),
                Tup(_) => write!(f, ""),
            }
        }
    }
}

#[derive(Debug)]
pub struct Codegen {
    pub module: inkwell::module::Module<'static>,
    pub target: inkwell::targets::TargetMachine,
}
impl Codegen {
    pub fn new() -> Self {
        let ctx = Box::leak(Box::new(inkwell::context::Context::create()));
        let module = ctx.create_module("main");
        targets::Target::initialize_native(&targets::InitializationConfig {
            asm_parser: true,
            asm_printer: true,
            base: true,
            disassembler: false,
            info: false,
            machine_code: true,
        }).unwrap();
        let triple = TargetMachine::get_default_triple();
        let target = targets::Target::from_triple(&triple).unwrap();
        let cpu = TargetMachine::get_host_cpu_name();
        let features = TargetMachine::get_host_cpu_features();
        let target = target.create_target_machine(
            &triple,
            cpu.to_str().unwrap(),
            features.to_str().unwrap(),
            inkwell::OptimizationLevel::None,
            targets::RelocMode::Default,
            targets::CodeModel::Default,
        ).unwrap();
        Self { module, target }
    }
}
