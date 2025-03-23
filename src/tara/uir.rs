use std::rc::Rc;

use super::{parse::Ident, preimport, Module, ModuleId, Tara};
use crate::{
    message,
    misc::{CheapClone, Indexer, Istr, Ivec, Svec},
    parse, MkIndexer, Provenance,
};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct In {
    pub m: ModuleId,
}

#[derive(Clone)]
pub struct Out {
    pub items: Rc<[Id]>,
}

impl Tara {
    pub fn resolve(&mut self, i: In) -> Out {
        match self.resolution.get(&i) {
            Some(Some(o)) => o.clone(),
            Some(None) => panic!("resolution entered a cycle!"),
            None => {
                self.resolution.insert(i, None);
                let data = resolve(self, i);
                self.resolution.insert(i, Some(data));
                self.resolution.get(&i).cloned().unwrap().unwrap()
            }
        }
    }
}

MkIndexer!(pub Id, u32);
impl Id {
    pub fn into_typecase(self) -> TypecaseId {
        TypecaseId(self)
    }
    pub fn into_typedecl(self) -> TypedeclId {
        TypedeclId(self)
    }
    pub fn into_func(self) -> FuncId {
        FuncId(self)
    }
}
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct TypedeclId(Id);
impl From<TypedeclId> for Id {
    fn from(v: TypedeclId) -> Self {
        v.0
    }
}
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct TypecaseId(Id);
impl From<TypecaseId> for Id {
    fn from(v: TypecaseId) -> Self {
        v.0
    }
}
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct FuncId(Id);
impl From<FuncId> for Id {
    fn from(v: FuncId) -> Self {
        v.0
    }
}
impl std::ops::Index<TypedeclId> for Ivec<Id, Item> {
    type Output = Typedecl;

    fn index(&self, index: TypedeclId) -> &Self::Output {
        match self[index.0] {
            Item::Typedecl(ref t) => t,
            _ => unreachable!(),
        }
    }
}
impl std::ops::IndexMut<TypedeclId> for Ivec<Id, Item> {
    fn index_mut(&mut self, index: TypedeclId) -> &mut Self::Output {
        match self[index.0] {
            Item::Typedecl(ref mut t) => t,
            _ => unreachable!(),
        }
    }
}
impl std::ops::Index<TypecaseId> for Ivec<Id, Interface> {
    type Output = TypecaseInter;

    fn index(&self, index: TypecaseId) -> &Self::Output {
        match self[index.0] {
            Interface::Typecase(ref t) => t,
            _ => unreachable!(),
        }
    }
}
impl std::ops::IndexMut<TypecaseId> for Ivec<Id, Interface> {
    fn index_mut(&mut self, index: TypecaseId) -> &mut Self::Output {
        match self[index.0] {
            Interface::Typecase(ref mut t) => t,
            _ => unreachable!(),
        }
    }
}
impl std::ops::Index<FuncId> for Ivec<Id, Item> {
    type Output = Function;

    fn index(&self, index: FuncId) -> &Self::Output {
        match self[index.0] {
            Item::Function(ref t) => t,
            _ => unreachable!(),
        }
    }
}
impl std::ops::IndexMut<FuncId> for Ivec<Id, Item> {
    fn index_mut(&mut self, index: FuncId) -> &mut Self::Output {
        match self[index.0] {
            Item::Function(ref mut t) => t,
            _ => unreachable!(),
        }
    }
}
impl std::ops::Index<FuncId> for Ivec<Id, Interface> {
    type Output = FunctionInter;

    fn index(&self, index: FuncId) -> &Self::Output {
        match self[index.0] {
            Interface::Function(ref f) => f,
            _ => unreachable!(),
        }
    }
}

MkIndexer!(pub LocalId, u32);
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct ExprId(LocalId);
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct BindingId(LocalId);
MkIndexer!(pub TypeId, u32);
type Typevec = Ivec<TypeId, Typekind>;

impl TypeId {
    pub fn replace(self, types: &mut Typevec, map: &std::collections::BTreeMap<usize, TypeId>) {
        match types[self].clone() {
            Typekind::Func { args, ret } => {
                args.replace(types, map);
                ret.replace(types, map);
            }
            Typekind::Call { args, func } => {
                func.replace(types, map);
                args.replace(types, map);
            }
            Typekind::Bundle(k_fields) => {
                for &k in k_fields.clone().iter() {
                    k.replace(types, map);
                }
            }
            Typekind::Var(v) => {
                if let Some(t) = map.get(&v) {
                    types[self] = types[*t].clone()
                }
            }
            _ => {}
        }
    }
}

#[derive(Default, Debug)]
pub struct LocalVec(Ivec<LocalId, Locals>);
impl LocalVec {
    pub fn promise_expr(&self) -> ExprId {
        ExprId(self.0.promise())
    }
    pub fn push_expr(&mut self, e: Expr) -> ExprId {
        ExprId(self.0.push(Locals::Expr(e)))
    }
    pub fn push_binding(&mut self, b: Binding) -> BindingId {
        BindingId(self.0.push(Locals::Binding(b)))
    }
}
impl std::ops::Index<LocalId> for LocalVec {
    type Output = Locals;

    fn index(&self, index: LocalId) -> &Self::Output {
        &self.0[index]
    }
}
impl std::ops::IndexMut<LocalId> for LocalVec {
    fn index_mut(&mut self, index: LocalId) -> &mut Self::Output {
        &mut self.0[index]
    }
}
impl std::ops::Index<ExprId> for LocalVec {
    type Output = Expr;

    fn index(&self, index: ExprId) -> &Self::Output {
        match &self[index.0] {
            Locals::Expr(e) => e,
            _ => unreachable!(),
        }
    }
}
impl std::ops::IndexMut<ExprId> for LocalVec {
    fn index_mut(&mut self, index: ExprId) -> &mut Self::Output {
        match &mut self[index.0] {
            Locals::Expr(e) => e,
            _ => unreachable!(),
        }
    }
}
impl std::ops::Index<BindingId> for LocalVec {
    type Output = Binding;

    fn index(&self, index: BindingId) -> &Self::Output {
        match &self[index.0] {
            Locals::Binding(b) => b,
            _ => unreachable!(),
        }
    }
}

impl Tara {
    fn push_expr(&mut self, locals: Id, e: Expr) -> ExprId {
        self.uir_locals[locals].push_expr(e)
    }
}

#[derive(Debug)]
pub enum Item {
    Typecase(Typecase),
    Typedecl(Typedecl),
    Function(Function),
    Namespace(Namespace),
}
impl Item {
    pub fn function(&mut self) -> &mut Function {
        match self {
            Item::Function(f) => f,
            _ => panic!(),
        }
    }
}

pub enum Interface {
    Typecase(TypecaseInter),
    Typedecl(TypedeclInter),
    Function(FunctionInter),
    Namespace(NamespaceInter),
}
impl Interface {
    pub fn into_namespace(&mut self) -> &mut NamespaceInter {
        match self {
            Interface::Namespace(n) => n,
            _ => panic!(),
        }
    }
    pub fn into_func_immut(&self) -> &FunctionInter {
        match self {
            Interface::Function(f) => f,
            _ => panic!(),
        }
    }
    pub fn into_func(&mut self) -> &mut FunctionInter {
        match self {
            Interface::Function(f) => f,
            _ => panic!(),
        }
    }
    pub fn name(&self) -> Ident {
        match self {
            Interface::Typedecl(t) => t.name,
            Interface::Function(f) => f.name,
            Interface::Namespace(n) => n.name,
            Interface::Typecase(c) => c.name,
        }
    }
}

#[derive(Debug)]
pub struct Namespace {}
pub struct NamespaceInter {
    pub name: Ident,
    pub items: Rc<Svec<Istr, Id>>,
}

#[derive(Debug)]
pub struct Typedecl {
    pub cases: Rc<[TypecaseId]>,
}
pub struct TypedeclInter {
    pub loc: Provenance,
    pub name: Ident,
}

#[derive(Debug)]
pub struct Typecase {}
#[derive(Clone, Copy)]
pub struct TypecaseInter {
    pub loc: Provenance,
    pub name: Ident,
    pub typ: Type,
    pub binding: BindingId,
    pub parent: TypedeclId,
    pub index: usize,
}

#[derive(Debug)]
pub struct Function {
    pub body: ExprId,
}
pub struct FunctionInter {
    pub loc: Provenance,
    pub name: Ident,
    pub typ: Type,
}

impl Function {
    pub fn fmt<'a>(
        &'a self,
        inter: &'a FunctionInter,
        locals: &'a LocalVec,
        inters: &'a Ivec<Id, Interface>,
        typevec: &'a Typevec,
        items: &'a Ivec<Id, Item>,
    ) -> FunctionFmt<'a> {
        FunctionFmt {
            func: self,
            typevec,
            locals,
            inters,
            inter,
            items,
        }
    }
}

pub struct FunctionFmt<'a> {
    func: &'a Function,
    inter: &'a FunctionInter,
    locals: &'a LocalVec,
    inters: &'a Ivec<Id, Interface>,
    typevec: &'a Typevec,
    items: &'a Ivec<Id, Item>,
}
impl std::fmt::Display for FunctionFmt<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = self.inter.name.name.0;
        let typ = self.inter.typ.fmt(self.inters, self.typevec);
        let body = self.locals[self.func.body].fmt(self.locals, self.inters, self.typevec, self.items, 0);
        write!(f, "func ({} ∈ {}) {}", name, typ, body)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Type {
    pub loc: Provenance,
    pub kind: TypeId,
}
impl Type {
    fn intern(typevec: &mut Typevec, kind: Typekind) -> TypeId {
        // typevec.push(kind)
        // something used to fuck this up, I'm guessing replace + multiple separate tvar counters?
        thread_local! {
            static STATE: std::cell::RefCell<std::collections::BTreeMap<Typekind, TypeId>>  = const { std::cell::RefCell::new(std::collections::BTreeMap::new()) }
        }
        STATE.with(|i| {
            let mut i = i.borrow_mut();
            if let Some(&d) = i.get(&kind) {
                return d;
            }
            let d = typevec.push(kind.clone());
            i.insert(kind, d);
            d
        })
    }

    pub fn fmt<'a>(
        &'a self,
        inters: &'a Ivec<Id, Interface>,
        typevec: &'a Typevec,
    ) -> TypekindFmt<'a> {
        typevec[self.kind].fmt(inters, typevec)
    }

    pub fn call(typevec: &mut Typevec, func: Type, args: Type, loc: Provenance) -> Type {
        let kind = Self::intern(
            typevec,
            Typekind::Call {
                func: func.kind,
                args: args.kind,
            },
        );
        Type { loc, kind }
    }

    pub fn func(typevec: &mut Typevec, args: Type, ret: Type, loc: Provenance) -> Type {
        let kind = Self::intern(
            typevec,
            Typekind::Func {
                args: args.kind,
                ret: ret.kind,
            },
        );
        Type { loc, kind }
    }

    pub fn argument(&self, typevec: &Typevec) -> Option<Type> {
        match typevec[self.kind] {
            Typekind::Func { args, .. } => Some(Type {
                kind: args,
                loc: self.loc,
            }),
            _ => None,
        }
    }

    pub fn ret(&self, typevec: &Typevec) -> Option<Type> {
        match typevec[self.kind] {
            Typekind::Func { ret, .. } => Some(Type {
                kind: ret,
                loc: self.loc,
            }),
            _ => None,
        }
    }

    pub fn simple(typevec: &mut Typevec, kind: Typekind, loc: Provenance) -> Type {
        let kind = Self::intern(typevec, kind);
        Type { loc, kind }
    }

    pub fn unit(typevec: &mut Typevec, loc: Provenance) -> Type {
        Self::tup(typevec, &[], loc)
    }

    pub fn tup(typevec: &mut Typevec, fields: &[Type], loc: Provenance) -> Type {
        let bundle = Type::bundle(typevec, fields, loc);
        let tup = Self::intern(typevec, Typekind::Tup);
        let kind = Self::intern(
            typevec,
            Typekind::Call {
                args: bundle.kind,
                func: tup,
            },
        );
        Type { kind, loc }
    }

    pub fn bundle(typevec: &mut Typevec, fields: &[Type], loc: Provenance) -> Type {
        let kind: Vec<_> = fields.iter().map(|f| f.kind).collect();
        let kind = Self::intern(typevec, Typekind::Bundle(kind.into()));
        Type { loc, kind }
    }
}
impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}
impl Eq for Type {}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Typekind {
    Func { args: TypeId, ret: TypeId },
    Call { args: TypeId, func: TypeId },
    Bundle(Rc<[TypeId]>),
    Recall(Id),
    Var(usize),
    String,
    Bool,
    Int,
    Tup,
}
impl CheapClone for Typekind {}
impl Typekind {
    pub fn fmt<'a>(
        &'a self,
        inters: &'a Ivec<Id, Interface>,
        typevec: &'a Typevec,
    ) -> TypekindFmt<'a> {
        TypekindFmt {
            kind: self,
            typevec,
            inters,
        }
    }
}

pub struct TypekindFmt<'a> {
    kind: &'a Typekind,
    inters: &'a Ivec<Id, Interface>,
    typevec: &'a Typevec,
}
impl TypekindFmt<'_> {
    fn sub(&self, id: TypeId) -> Self {
        self.typevec[id].fmt(self.inters, self.typevec)
    }
}
impl std::fmt::Display for TypekindFmt<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            Typekind::Func { args, ret } => {
                write!(f, "func({}): {}", self.sub(*args), self.sub(*ret),)
            }
            Typekind::Call { args, func } => write!(f, "{}{}", self.sub(*func), self.sub(*args),),
            Typekind::Bundle(items) => {
                write!(f, "(")?;
                if !items.is_empty() {
                    write!(f, "{}", self.sub(items[0]))?;
                    for &i in &items[1..] {
                        write!(f, ", {}", self.sub(i))?;
                    }
                }
                write!(f, ")")
            }
            Typekind::Recall(id) => write!(f, "{}", self.inters[*id].name().name.0),
            Typekind::Var(i) => write!(f, "?{}", i),
            Typekind::String => write!(f, "string"),
            Typekind::Bool => write!(f, "bool"),
            Typekind::Int => write!(f, "int"),
            Typekind::Tup => write!(f, "tuple"),
        }
    }
}

#[derive(Debug)]
pub enum Locals {
    Expr(Expr),
    Binding(Binding),
}

#[derive(Debug)]
pub struct Expr {
    pub loc: Provenance,
    pub typ: Type,
    pub kind: Exprkind,
}
impl Expr {
    pub fn fmt<'a>(
        &'a self,
        locals: &'a LocalVec,
        inters: &'a Ivec<Id, Interface>,
        typevec: &'a Typevec,
        items: &'a Ivec<Id, Item>,
        nesting: usize,
    ) -> ExprFmt<'a> {
        ExprFmt {
            expr: self,
            locals,
            inters,
            typevec,
            items,
            nesting,
        }
    }
}

pub struct ExprFmt<'a> {
    expr: &'a Expr,
    locals: &'a LocalVec,
    inters: &'a Ivec<Id, Interface>,
    typevec: &'a Typevec,
    items: &'a Ivec<Id, Item>,
    nesting: usize,
}
impl ExprFmt<'_> {
    fn sub(&self, id: ExprId) -> Self {
        self.locals[id].fmt(
            self.locals,
            self.inters,
            self.typevec,
            self.items,
            self.nesting + 1,
        )
    }
}
impl std::fmt::Display for ExprFmt<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let typ = self.expr.typ.fmt(self.inters, self.typevec);
        match self.expr.kind {
            Exprkind::If { cond, smash, pass } => {
                let cond = self.sub(cond);
                let smash = self.sub(smash);
                let pass = self.sub(pass);
                write!(f, "(if {} {} else {} ∈ {})", cond, smash, pass, typ)
            }
            Exprkind::Call { func, args } => {
                let func = self.sub(func);
                let args = self.sub(args);
                write!(f, "({}{} ∈ {})", func, args, typ)
            }
            Exprkind::Builtin(builtin) => write!(f, "({} ∈ {})", builtin, typ),
            Exprkind::Tuple(ref expr_ids) => {
                write!(f, "(")?;
                if let Some(&e1) = expr_ids.first() {
                    let e1 = self.sub(e1);
                    write!(f, "{}", e1)?;
                    for &ex in &expr_ids[1..] {
                        let ex = self.sub(ex);
                        write!(f, ", {}", ex)?;
                    }
                }
                write!(f, ")")
            }
            Exprkind::Loop(expr_id) => {
                let body = self.sub(expr_id);
                write!(f, "(loop {} ∈ {})", body, typ)
            }
            Exprkind::Bareblock(ref expr_ids) => {
                writeln!(f, "({{")?;
                for &e in expr_ids.as_ref() {
                    for _ in 0..self.nesting + 1 {
                        write!(f, "  ")?;
                    }
                    let e = self.sub(e);
                    writeln!(f, "{}", e)?;
                }
                for _ in 0..self.nesting {
                    write!(f, "  ")?;
                }
                write!(f, "}})")
            }
            Exprkind::Recall(binding_id) => {
                let binding = match binding_id {
                    Ok(b) => {
                        if let Bindkind::Name(n, _) = self.locals[b].kind {
                            n.0
                        } else {
                            unreachable!()
                        }
                    }
                    Err(i) => self.inters[i].name().name.0,
                };
                write!(f, "({} ∈ {})", binding, typ)
            }
            Exprkind::Number(istr) | Exprkind::Bool(istr) => write!(f, "{}", istr.0),
            Exprkind::String(istr) => write!(f, "{:?}", istr.0),
            Exprkind::Arguments => write!(f, "Args"),
            Exprkind::Poison => write!(f, "(!!)"),
            Exprkind::Let(binding_id, expr_id) => {
                let binding = self.locals[binding_id].fmt(self.locals, self.inters, self.typevec);
                let expr = self.sub(expr_id);
                write!(f, "let {} = {}", binding, expr)
            }
            Exprkind::Assign(binding_id, expr_id) => {
                let binding = self.locals[binding_id].fmt(self.locals, self.inters, self.typevec);
                let expr = self.sub(expr_id);
                write!(f, "{} = {}", binding, expr)
            }
            Exprkind::Break { val, .. } => write!(f, "break {}", self.sub(val)),
            Exprkind::Return(expr_id) => write!(f, "return {}", self.sub(expr_id)),
            Exprkind::Const(expr_id) => write!(f, "{};", self.sub(expr_id)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Exprkind {
    If {
        cond: ExprId,
        smash: ExprId,
        pass: ExprId,
    },
    Call {
        func: ExprId,
        args: ExprId,
    },
    Builtin(Builtinkind),
    Tuple(Rc<[ExprId]>),
    Loop(ExprId),
    Bareblock(Rc<[ExprId]>),
    // add constructors as thingy
    Recall(Result<BindingId, Id>),
    Number(Istr),
    String(Istr),
    Bool(Istr),
    Arguments,
    Poison,

    Let(BindingId, ExprId),
    Assign(BindingId, ExprId),
    Break {
        val: ExprId,
        target: ExprId,
    },
    Return(ExprId),
    Const(ExprId),
}
impl Default for Exprkind {
    fn default() -> Self {
        Exprkind::Tuple(Rc::new([]))
    }
}

macro_rules! MkSimpleEnum {
    ($vis:vis $enum:ident : $($name:ident),* $(,)?) => {
        #[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
        $vis enum $enum {
            $($name),*
        }
        impl $enum {
            pub const VALUES: &'static [$enum] = {
                use $enum::*;
                &[
                    $($name),*
                ]
            };
        }
    };
}

MkSimpleEnum! {pub Builtinkind:
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
impl Builtinkind {
    pub const fn spelling(self) -> &'static str {
        match self {
            Builtinkind::Add => "__builtin_add",
            Builtinkind::Sub => "__builtin_sub",
            Builtinkind::Mul => "__builtin_mul",
            Builtinkind::Div => "__builtin_div",
            Builtinkind::Mod => "__builtin_mod",
            Builtinkind::And => "__builtin_and",
            Builtinkind::Or => "__builtin_or",
            Builtinkind::Xor => "__builtin_xor",
            Builtinkind::ShLeft => "__builtin_shl",
            Builtinkind::ShRight => "__builtin_shr",
            Builtinkind::Not => "__builtin_not",
            Builtinkind::Negate => "__builtin_negate",
            Builtinkind::CmpEq => "__builtin_cmp_eq",
            Builtinkind::CmpNE => "__builtin_cmp_ne",
            Builtinkind::CmpGt => "__builtin_cmp_gt",
            Builtinkind::CmpLt => "__builtin_cmp_lt",
            Builtinkind::CmpGE => "__builtin_cmp_ge",
            Builtinkind::CmpLE => "__builtin_cmp_le",
            Builtinkind::Syscall => "__builtin_syscall",
            Builtinkind::PtrToInt => "__builtin_ptr_to_int",
            Builtinkind::IntToPtr => "__builtin_int_to_ptr",
        }
    }
}
impl std::fmt::Display for Builtinkind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.spelling())
    }
}

#[derive(Debug)]
pub struct Binding {
    pub loc: Provenance,
    pub typ: Type,
    pub kind: Bindkind,
}
impl Binding {
    pub fn fmt<'a>(
        &'a self,
        locals: &'a LocalVec,
        inters: &'a Ivec<Id, Interface>,
        typevec: &'a Typevec,
    ) -> BindingFmt<'a> {
        BindingFmt {
            bind: self,
            locals,
            inters,
            typevec,
        }
    }
}

pub struct BindingFmt<'a> {
    bind: &'a Binding,
    locals: &'a LocalVec,
    inters: &'a Ivec<Id, Interface>,
    typevec: &'a Typevec,
}
impl std::fmt::Display for BindingFmt<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let typ = self.bind.typ.fmt(self.inters, self.typevec);
        match self.bind.kind {
            Bindkind::Empty => write!(f, "(∅ ∈ {})", typ),
            Bindkind::Name(istr, m) => {
                write!(f, "({} ∈ {}{})", istr.0, if m { "mut " } else { "" }, typ)
            }
            Bindkind::Tuple(ref binding_ids) => {
                write!(f, "(")?;
                if let Some(&b1) = binding_ids.first() {
                    let b1 = self.locals[b1].fmt(self.locals, self.inters, self.typevec);
                    write!(f, "{}", b1)?;
                    for &bx in &binding_ids[1..] {
                        let bx = self.locals[bx].fmt(self.locals, self.inters, self.typevec);
                        write!(f, ", {}", bx)?;
                    }
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Debug)]
pub enum Bindkind {
    Empty,
    Name(Istr, bool),
    Tuple(Rc<[BindingId]>),
}

fn resolve(tara: &mut Tara, i: In) -> Out {
    let imports = tara.preimport(preimport::In { m: i.m }).imports;
    let ast = tara.parse(parse::In { m: i.m });
    let mut globals = Svec::default();
    for i in imports.iter() {
        let items = tara.resolve(In { m: i.target }).items;
        if let Some(decl) = i.head {
            for &id in items.iter() {
                if tara.item_name(id).name != decl {
                    continue;
                }
                globals.insert(tara.item_name(id).name, id);
                break;
            }
        } else {
            for &id in items.iter() {
                globals.insert(tara.item_name(id).name, id);
            }
        }
    }
    let mut items = Vec::new();
    for i in ast.ast.types.iter() {
        let locals = LocalVec::default();
        let locals = tara.uir_locals.push(locals);
        let inter = i.interface();
        let id = locals;
        globals.insert(inter.name.name, id);
        tara.uir_interfaces
            .register(Interface::Typedecl(inter), id)
            .expect("auegh");
        items.push(id);
    }

    for (idx, typ) in ast.ast.types.iter().enumerate() {
        let id = items[idx];
        let item = typ.convert(id.into_typedecl(), &globals, tara);
        // converting the cases creates new items
        items.extend(item.cases.iter().copied().map(Into::<Id>::into));
        tara.uir_items
            .register(Item::Typedecl(item), id)
            .expect(", ich bin in gefahr");
    }

    let funcs_start = items.len();
    for i in ast.ast.funcs.iter() {
        let locals = LocalVec::default();
        let locals = tara.uir_locals.push(locals);
        let inter = i.interface(&globals, tara);
        let id = locals;
        globals.insert(inter.name.name, id);
        tara.uir_interfaces
            .register(Interface::Function(inter), id)
            .expect("yaeh");
        items.push(id);
    }

    // technically we could create a new names vec each time
    // (which wasn't possible before as it also stored the globals)
    // but I'm guessing this saves a few allocations, since the capacity is reused
    let mut names = Svec::default();
    for (i, &locals) in ast.ast.funcs.iter().zip(items[funcs_start..].iter()) {
        let item = names.excursion(|names| i.convert(locals, &globals, names, tara));
        let id = locals;
        tara.uir_items
            .register(Item::Function(item), id)
            .expect("yaeh");
    }
    Out {
        items: items.into(),
    }
}

struct Tctx<'a> {
    id: Id,
    globals: &'a Svec<Istr, Id>,
    tara: &'a mut Tara,
}
impl<'a> BindingCtxT<'a> for Tctx<'a> {
    fn globals(&self) -> &'a Svec<Istr, Id> {
        self.globals
    }
    fn modules(&self) -> &Ivec<ModuleId, Module> {
        &self.tara.modules
    }
    // fn types(&mut self) -> &mut Typevec {
    //     &mut self.tara.uir_types
    // }
    fn tara(&mut self) -> &mut Tara {
        self.tara
    }
    fn empty_type(&mut self) -> TypeId {
        let bundle = Type::intern(&mut self.tara.uir_types, Typekind::Bundle(Rc::new([])));
        let tup = Type::intern(&mut self.tara.uir_types, Typekind::Tup);
        Type::intern(
            &mut self.tara.uir_types,
            Typekind::Call {
                args: bundle,
                func: tup,
            },
        )
    }
    fn no_type(&mut self) -> Option<TypeId> {
        None
    }
    fn locals(&mut self) -> &mut LocalVec {
        &mut self.tara.uir_locals[self.id]
    }
    fn push_name(&mut self, _: Istr, _: BindingId) {}
}

impl parse::Typedecl {
    fn interface(&self) -> TypedeclInter {
        let name = self.name;
        let loc = self.loc;
        TypedeclInter { loc, name }
    }
    fn convert(&self, id: TypedeclId, globals: &Svec<Istr, Id>, tara: &mut Tara) -> Typedecl {
        let mut cases = Vec::with_capacity(self.cases.len());
        for (index, case) in self.cases.iter().enumerate() {
            let locals = LocalVec::default();
            let locals = tara.uir_locals.push(locals);
            let inter = case.interface(id, index, locals, globals, tara);
            tara.uir_interfaces
                .register(Interface::Typecase(inter), locals)
                .expect("bitte helfen Sie mir");
            cases.push(locals.into_typecase());
        }
        Typedecl {
            cases: cases.into(),
        }
    }
}

impl parse::Typecase {
    fn interface(
        &self,
        parent: TypedeclId,
        index: usize,
        locals: Id,
        globals: &Svec<Istr, Id>,
        tara: &mut Tara,
    ) -> TypecaseInter {
        let binding = self.binding.convert(
            false,
            &mut Tctx {
                id: locals,
                globals,
                tara,
            },
        );
        let args = tara[(locals, binding)].typ;
        let ret = Type::simple(
            &mut tara.uir_types,
            Typekind::Recall(parent.into()),
            self.loc,
        );
        let typ = Type::func(&mut tara.uir_types, args, ret, self.loc);
        TypecaseInter {
            name: self.name,
            loc: self.loc,
            binding,
            parent,
            index,
            typ,
        }
    }
}

impl parse::Function {
    fn interface(&self, globals: &Svec<Istr, Id>, tara: &mut Tara) -> FunctionInter {
        let name = self.name;
        let loc = self.loc;
        let ret = self.ret.convert(globals, tara);
        let arg = self.args.typing(globals, tara);
        let typ = Type::func(&mut tara.uir_types, arg, ret, loc);
        FunctionInter { loc, typ, name }
    }

    fn convert(
        &self,
        id: Id,
        globals: &Svec<Istr, Id>,
        names: &mut Svec<Istr, BindingId>,
        tara: &mut Tara,
    ) -> Function {
        let bind = self.args.convert(
            false,
            &mut Ectx {
                loops: Vec::new(),
                globals,
                names,
                tara,
                id,
            },
        );
        let block = self.body.convert(&mut Ectx {
            loops: Vec::new(),
            globals,
            names,
            tara,
            id,
        });
        let args = Expr {
            loc: self.args.loc,
            typ: tara.uir_interfaces[id]
                .into_func()
                .typ
                .argument(&tara.uir_types)
                .unwrap(),
            kind: Exprkind::Arguments,
        };
        let args = tara.push_expr(id, args);
        let bind = Expr {
            loc: self.args.loc,
            typ: Type::unit(&mut tara.uir_types, self.args.loc),
            kind: Exprkind::Let(bind, args),
        };
        let bind = tara.push_expr(id, bind);
        let body = Expr {
            loc: self.args.loc,
            typ: tara.uir_locals[id][block].typ,
            kind: Exprkind::Bareblock(Rc::new([bind, block])),
        };
        let body = tara.uir_locals[id].push_expr(body);
        Function { body }
    }
}

pub struct Ectx<'a> {
    id: Id,
    loops: Vec<ExprId>,
    globals: &'a Svec<Istr, Id>,
    names: &'a mut Svec<Istr, BindingId>,
    tara: &'a mut Tara,
}
impl Ectx<'_> {
    fn get_name(&self, n: Istr, loc: Option<Provenance>) -> Option<Result<BindingId, Id>> {
        match self
            .names
            .find(&n)
            .copied()
            .map(Ok)
            .or_else(|| self.globals.find(&n).copied().map(Err))
        {
            Some(id) => Some(id),
            None => {
                self.tara.report(
                    message!(error @? loc => "Could not resolve the following name!"),
                    &[],
                );
                None
            }
        }
    }
}
impl Tara {
    fn new_typevar(&mut self) -> TypeId {
        let kind = Typekind::Var(self.uir_tvars);
        self.uir_tvars += 1;
        self.uir_types.push(kind)
    }
    pub fn item_type(&self, i: Id, loc: Option<Provenance>) -> Type {
        match self.uir_interfaces[i] {
            Interface::Typedecl(ref t) => {
                self.report(
                    message!(
                        error @? loc =>
                        "Expected a function, but the name '{}' refers to a type!",
                        t.name.name.0
                    ),
                    &[message!(note @ t.loc => "Type declared here:")],
                );
                std::process::exit(1);
            }
            Interface::Namespace(ref t) => {
                self.report(
                    message!(
                        error @? loc =>
                        "Expected a function, but the name '{}' refers to a namespace!",
                        t.name.name.0
                    ),
                    &[],
                );
                std::process::exit(1);
            }
            Interface::Function(ref f) => f.typ,
            Interface::Typecase(ref c) => c.typ,
        }
    }
}
impl<'a> BindingCtxT<'a> for Ectx<'a> {
    fn empty_type(&mut self) -> TypeId {
        self.tara.new_typevar()
    }
    fn modules(&self) -> &Ivec<ModuleId, Module> {
        &self.tara.modules
    }
    // fn types(&mut self) -> &mut Typevec {
    //     &mut self.tara.uir_types
    // }
    fn tara(&mut self) -> &mut Tara {
        self.tara
    }
    fn no_type(&mut self) -> Option<TypeId> {
        Some(self.tara.new_typevar())
    }
    fn locals(&mut self) -> &mut LocalVec {
        &mut self.tara.uir_locals[self.id]
    }
    fn push_name(&mut self, n: Istr, b: BindingId) {
        self.names.insert(n, b)
    }
    fn globals(&self) -> &'a Svec<Istr, Id> {
        self.globals
    }
}

pub trait BindingCtxT<'a> {
    fn modules(&self) -> &Ivec<ModuleId, Module>;
    // fn types(&mut self) -> &mut Typevec;
    fn tara(&mut self) -> &mut Tara;
    fn globals(&self) -> &'a Svec<Istr, Id>;
    fn empty_type(&mut self) -> TypeId;
    fn no_type(&mut self) -> Option<TypeId>;
    fn locals(&mut self) -> &mut LocalVec;
    fn push_name(&mut self, _: Istr, _: BindingId);
}

pub trait BindingT: std::fmt::Debug {
    fn convert(&self, _: Provenance, mutable: bool, _: &mut dyn BindingCtxT) -> BindingId;
    fn typing(&self, _: Provenance, _: &Svec<Istr, Id>, _: &mut Tara) -> Type;
}

impl parse::Binding {
    pub fn convert(&self, mutable: bool, ctx: &mut dyn BindingCtxT) -> BindingId {
        self.kind.convert(self.loc, mutable, ctx)
    }
    pub fn typing(&self, globals: &Svec<Istr, Id>, ctx: &mut Tara) -> Type {
        self.kind.typing(self.loc, globals, ctx)
    }
}

impl BindingT for parse::binding::Empty {
    fn typing(&self, loc: Provenance, _: &Svec<Istr, Id>, ctx: &mut Tara) -> Type {
        Type::unit(&mut ctx.uir_types, loc)
    }
    fn convert(&self, loc: Provenance, _: bool, ctx: &mut dyn BindingCtxT) -> BindingId {
        let typ = Type {
            kind: ctx.empty_type(),
            loc,
        };
        ctx.locals().push_binding(Binding {
            kind: Bindkind::Empty,
            typ,
            loc,
        })
    }
}

impl BindingT for parse::binding::Name {
    fn typing(&self, loc: Provenance, globals: &Svec<Istr, Id>, tara: &mut Tara) -> Type {
        match self.1 {
            Some(ref t) => t.convert(globals, tara),
            None => {
                tara.report(
                    message!(error @ loc => "Found an unannotated binding in an invalid context!"),
                    &[message!(note => "Add a type annotation!")],
                );
                std::process::exit(1);
            }
        }
    }
    fn convert(&self, loc: Provenance, mutable: bool, ctx: &mut dyn BindingCtxT) -> BindingId {
        let typ = match self.1 {
            Some(ref t) => t.convert(ctx.globals(), ctx.tara()),
            None => match ctx.no_type() {
                Some(kind) => Type { kind, loc },
                None => {
                    ctx.tara().report(
                        message!(
                            error @ loc =>
                            "Found an unannotated binding in an invalid context!",
                        ),
                        &[message!(note => "Add a type annotation!")],
                    );
                    std::process::exit(1);
                }
            },
        };
        let bid = ctx.locals().push_binding(Binding {
            kind: Bindkind::Name(self.0, mutable),
            typ,
            loc,
        });
        ctx.push_name(self.0, bid);
        bid
    }
}

impl BindingT for parse::binding::Tuple {
    fn typing(&self, loc: Provenance, globals: &Svec<Istr, Id>, ctx: &mut Tara) -> Type {
        let ts: Vec<_> = self.0.iter().map(|b| b.typing(globals, ctx)).collect();
        Type::tup(&mut ctx.uir_types, &ts, loc)
    }
    fn convert(&self, loc: Provenance, mutable: bool, ctx: &mut dyn BindingCtxT) -> BindingId {
        let (ts, bs): (Vec<_>, Vec<_>) = self
            .0
            .iter()
            .map(|b| {
                let b = b.convert(mutable, ctx);
                (ctx.locals()[b].typ, b)
            })
            .unzip();
        let typ = Type::tup(&mut ctx.tara().uir_types, &ts, loc);
        ctx.locals().push_binding(Binding {
            kind: Bindkind::Tuple(bs.into()),
            typ,
            loc,
        })
    }
}

pub trait ExprT: std::fmt::Debug {
    fn convert(&self, _: Provenance, _: &mut Ectx) -> ExprId;
}

impl parse::Expr {
    pub fn convert(&self, ctx: &mut Ectx) -> ExprId {
        self.kind.convert(self.loc, ctx)
    }
}

impl ExprT for parse::expr::If {
    fn convert(&self, loc: Provenance, ctx: &mut Ectx) -> ExprId {
        let cond = self.cond.convert(ctx);
        let smash = self.smash.convert(ctx);
        let pass = self.pass.convert(ctx);
        let typ = ctx.locals()[smash].typ;
        ctx.locals().push_expr(Expr {
            kind: Exprkind::If { cond, smash, pass },
            typ,
            loc,
        })
    }
}

impl ExprT for parse::expr::Call {
    fn convert(&self, loc: Provenance, ctx: &mut Ectx) -> ExprId {
        let func = self.func.convert(ctx);
        let args = self.args.convert(ctx);
        let typ = Type {
            kind: ctx.tara.new_typevar(),
            loc,
        };
        ctx.locals().push_expr(Expr {
            kind: Exprkind::Call { func, args },
            typ,
            loc,
        })
    }
}

impl ExprT for parse::expr::Tuple {
    fn convert(&self, loc: Provenance, ctx: &mut Ectx) -> ExprId {
        let (types, exprs): (Vec<_>, Vec<_>) = self
            .0
            .iter()
            .map(|e| {
                let e = e.convert(ctx);
                (ctx.locals()[e].typ, e)
            })
            .unzip();
        let typ = Type::tup(&mut ctx.tara.uir_types, &types, loc);
        ctx.locals().push_expr(Expr {
            kind: Exprkind::Tuple(exprs.into()),
            typ,
            loc,
        })
    }
}

impl ExprT for parse::expr::Loop {
    fn convert(&self, loc: Provenance, ctx: &mut Ectx) -> ExprId {
        let slot = Expr {
            typ: Type::unit(&mut ctx.tara.uir_types, loc),
            kind: Exprkind::default(),
            loc,
        };
        let slot = ctx.locals().push_expr(slot);
        ctx.loops.push(slot);
        let expr = self.0.convert(ctx);
        ctx.loops.pop();
        let typ = Type {
            kind: ctx.tara.new_typevar(),
            loc,
        };
        ctx.locals()[slot] = Expr {
            kind: Exprkind::Loop(expr),
            typ,
            loc,
        };
        slot
    }
}

impl ExprT for parse::expr::Bareblock {
    fn convert(&self, loc: Provenance, ctx: &mut Ectx) -> ExprId {
        let exprs: Vec<_> = self.0.iter().map(|e| e.convert(ctx)).collect();
        let typ = exprs
            .last()
            .map(|&id| ctx.locals()[id].typ)
            .unwrap_or_else(|| Type {
                kind: ctx.tara.new_typevar(),
                loc,
            });
        ctx.locals().push_expr(Expr {
            kind: Exprkind::Bareblock(exprs.into()),
            typ,
            loc,
        })
    }
}

impl ExprT for parse::expr::Recall {
    fn convert(&self, loc: Provenance, ctx: &mut Ectx) -> ExprId {
        for &builtin in Builtinkind::VALUES {
            if builtin.spelling() == self.0 .0 {
                let typ = Type {
                    kind: ctx.tara.new_typevar(),
                    loc,
                };
                return ctx.locals().push_expr(Expr {
                    kind: Exprkind::Builtin(builtin),
                    typ,
                    loc,
                });
            }
        }

        match ctx.get_name(self.0, Some(loc)) {
            Some(id) => {
                let typ = id
                    .map_err(|e| ctx.tara.item_type(e, Some(loc)))
                    .map(|l| ctx.locals()[l].typ)
                    .unwrap_or_else(|e| e);
                ctx.locals().push_expr(Expr {
                    kind: Exprkind::Recall(id),
                    typ,
                    loc,
                })
            }
            None => {
                let typ = Type {
                    kind: ctx.tara.new_typevar(),
                    loc,
                };
                ctx.locals().push_expr(Expr {
                    kind: Exprkind::Poison,
                    typ,
                    loc,
                })
            }
        }
    }
}

impl ExprT for parse::expr::Path {
    fn convert(&self, old_loc: Provenance, ctx: &mut Ectx) -> ExprId {
        let poison = |ctx: &mut Ectx| {
            let typ = Type {
                kind: ctx.tara.new_typevar(),
                loc: old_loc,
            };
            ctx.locals().push_expr(Expr {
                kind: Exprkind::Poison,
                loc: old_loc,
                typ,
            })
        };
        let loc = self.0[0].loc;
        match ctx.get_name(self.0[0].name, Some(loc)) {
            None => poison(ctx),
            Some(Ok(bid)) => {
                let bid_loc = ctx.tara[(ctx.id, bid)].loc;
                ctx.tara.report(
                    message!(error @ loc => "Expected a namespace, but got a local binding!"),
                    &[message!(note @ bid_loc => "Local binding declared here!")],
                );
                poison(ctx)
            }
            Some(Err(mut id)) => {
                for n in &self.0[1..] {
                    match ctx.tara.uir_interfaces[id] {
                        Interface::Typedecl(ref t) => {
                            ctx.tara.report(
                                message!(error @ n.loc => "Expected a namespace, but got a type!"),
                                &[message!(note @ t.name.loc => "Type declared here!")],
                            );
                            return poison(ctx);
                        }
                        Interface::Typecase(ref c) => {
                            ctx.tara.report(
                                message!(error @ n.loc => "Expected a namespace, but got a constructor!"),
                                &[message!(note @ c.name.loc => "Constuctor declared here!")],
                            );
                            return poison(ctx);
                        }
                        Interface::Function(ref f) => {
                            ctx.tara.report(
                                message!(error @ n.loc => "Expected a namespace, but got a function!"),
                                &[message!(note @ f.name.loc => "Function declared here!")],
                            );
                            return poison(ctx);
                        }
                        Interface::Namespace(ref s) => match s.items.find(&n.name) {
                            Some(&i) => id = i,
                            None => {
                                ctx.tara.report(
                                    message!(error @ n.loc => "Could not resolve the following name!"),
                                    &[],
                                );
                                return poison(ctx);
                            }
                        },
                    }
                }
                let typ = ctx.tara.item_type(id, Some(self.0.last().unwrap().loc));
                ctx.locals().push_expr(Expr {
                    kind: Exprkind::Recall(Err(id)),
                    loc: old_loc,
                    typ,
                })
            }
        }
    }
}

impl ExprT for parse::expr::Number {
    fn convert(&self, loc: Provenance, ctx: &mut Ectx) -> ExprId {
        let typ = Type::simple(&mut ctx.tara.uir_types, Typekind::Int, loc);
        ctx.locals().push_expr(Expr {
            kind: Exprkind::Number(self.0),
            typ,
            loc,
        })
    }
}

impl ExprT for parse::expr::String {
    fn convert(&self, loc: Provenance, ctx: &mut Ectx) -> ExprId {
        let typ = Type::simple(&mut ctx.tara.uir_types, Typekind::String, loc);
        ctx.locals().push_expr(Expr {
            kind: Exprkind::String(self.0),
            typ,
            loc,
        })
    }
}

impl ExprT for parse::expr::Bool {
    fn convert(&self, loc: Provenance, ctx: &mut Ectx) -> ExprId {
        let typ = Type::simple(&mut ctx.tara.uir_types, Typekind::Bool, loc);
        ctx.locals().push_expr(Expr {
            kind: Exprkind::Bool(self.0),
            typ,
            loc,
        })
    }
}

impl ExprT for parse::expr::Let {
    fn convert(&self, loc: Provenance, ctx: &mut Ectx) -> ExprId {
        let binding = self.0.convert(false, ctx);
        let expr = self.1.convert(ctx);
        let typ = Type::unit(&mut ctx.tara.uir_types, loc);
        ctx.locals().push_expr(Expr {
            kind: Exprkind::Let(binding, expr),
            typ,
            loc,
        })
    }
}

impl ExprT for parse::expr::Mut {
    fn convert(&self, loc: Provenance, ctx: &mut Ectx) -> ExprId {
        let binding = self.0.convert(true, ctx);
        let expr = self.1.convert(ctx);
        let typ = Type::unit(&mut ctx.tara.uir_types, loc);
        ctx.locals().push_expr(Expr {
            kind: Exprkind::Let(binding, expr),
            typ,
            loc,
        })
    }
}

impl ExprT for parse::expr::Assign {
    fn convert(&self, loc: Provenance, ctx: &mut Ectx) -> ExprId {
        let typ = Type::unit(&mut ctx.tara.uir_types, loc);
        let kind = match ctx.names.find(&self.0.name) {
            Some(bid) => {
                let bid = *bid;
                let expr = self.1.convert(ctx);
                Exprkind::Assign(bid, expr)
            }
            None => {
                ctx.tara.report(
                    message!(error @ loc => "Could not resolve the following local name!"),
                    &[],
                );
                self.1.convert(ctx);
                Exprkind::Poison
            }
        };
        ctx.locals().push_expr(Expr { kind, typ, loc })
    }
}

impl ExprT for parse::expr::Break {
    fn convert(&self, loc: Provenance, ctx: &mut Ectx) -> ExprId {
        let typ = Type::unit(&mut ctx.tara.uir_types, loc);
        let kind = match ctx.loops.last() {
            Some(&target) => {
                let val = if let Some(ref expr) = self.0 {
                    expr.convert(ctx)
                } else {
                    let typ = Type::unit(&mut ctx.tara.uir_types, loc);
                    ctx.locals().push_expr(Expr {
                        kind: Exprkind::default(),
                        typ,
                        loc,
                    })
                };
                Exprkind::Break { val, target }
            }
            None => {
                ctx.tara.report(
                    message!(error @ loc => "This break is not contained by any loops!"),
                    &[],
                );
                Exprkind::Poison
            }
        };
        ctx.locals().push_expr(Expr { kind, typ, loc })
    }
}

impl ExprT for parse::expr::Return {
    fn convert(&self, loc: Provenance, ctx: &mut Ectx) -> ExprId {
        let typ = Type::unit(&mut ctx.tara.uir_types, loc);
        let kind = match self.0 {
            None => Exprkind::default(),
            Some(ref expr) => Exprkind::Return(expr.convert(ctx)),
        };
        ctx.locals().push_expr(Expr { kind, typ, loc })
    }
}

impl ExprT for parse::expr::Const {
    fn convert(&self, loc: Provenance, ctx: &mut Ectx) -> ExprId {
        let typ = Type::unit(&mut ctx.tara.uir_types, loc);
        let kind = Exprkind::Const(self.0.convert(ctx));
        ctx.locals().push_expr(Expr { kind, typ, loc })
    }
}

impl parse::Type {
    fn convert(&self, globals: &Svec<Istr, Id>, tara: &mut Tara) -> Type {
        match &self.kind {
            parse::Typekind::Func { args, ret } => {
                let args = args.convert(globals, tara);
                let ret = ret.convert(globals, tara);
                Type::func(&mut tara.uir_types, args, ret, self.loc)
            }
            parse::Typekind::Call { args, func } => {
                let func = func.convert(globals, tara);
                let args = args.convert(globals, tara);
                Type::call(&mut tara.uir_types, func, args, self.loc)
            }
            parse::Typekind::Bundle(items) => {
                let items: Vec<_> = items.iter().map(|t| t.convert(globals, tara)).collect();
                Type::bundle(&mut tara.uir_types, &items, self.loc)
            }
            parse::Typekind::Recall(istr) => {
                let kind = match *istr {
                    s if s == "int".into() => Typekind::Int,
                    s if s == "string".into() => Typekind::String,
                    s if s == "bool".into() => Typekind::Bool,
                    s if s == "tuple".into() => Typekind::Tup,
                    s => match globals.find(&s) {
                        Some(&id) => Typekind::Recall(id),
                        None => {
                            tara.report(
                                message!(error @ self.loc => "Could not resolve the following typename!"),
                                &[],
                            );
                            return Type {
                                kind: tara.new_typevar(),
                                loc: self.loc,
                            };
                        }
                    },
                };
                Type::simple(&mut tara.uir_types, kind, self.loc)
            }
        }
    }
}
