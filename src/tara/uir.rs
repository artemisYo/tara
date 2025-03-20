use std::rc::Rc;

use super::{parse::Ident, preimport, Module, ModuleId, Tara};
use crate::{
    misc::{Indexer, Istr, Ivec, Svec},
    parse, report, Message, MkIndexer, Provenance,
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
    pub fn into_typedecl(self) -> TypedeclId {
        TypedeclId(self)
    }
    pub fn into_func(self) -> FuncId {
        FuncId(self)
    }
}
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct TypedeclId(Id);
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
            Item::Function(_) => unreachable!(),
        }
    }
}
impl std::ops::IndexMut<TypedeclId> for Ivec<Id, Item> {
    fn index_mut(&mut self, index: TypedeclId) -> &mut Self::Output {
        match self[index.0] {
            Item::Typedecl(ref mut t) => t,
            Item::Function(_) => unreachable!(),
        }
    }
}
impl std::ops::Index<FuncId> for Ivec<Id, Item> {
    type Output = Function;

    fn index(&self, index: FuncId) -> &Self::Output {
        match self[index.0] {
            Item::Function(ref t) => t,
            Item::Typedecl(_) => unreachable!(),
        }
    }
}
impl std::ops::IndexMut<FuncId> for Ivec<Id, Item> {
    fn index_mut(&mut self, index: FuncId) -> &mut Self::Output {
        match self[index.0] {
            Item::Function(ref mut t) => t,
            Item::Typedecl(_) => unreachable!(),
        }
    }
}
impl std::ops::Index<FuncId> for Ivec<Id, Interface> {
    type Output = FunctionInter;

    fn index(&self, index: FuncId) -> &Self::Output {
        match self[index.0] {
            Interface::Function(ref f) => f,
            Interface::Typedecl(_) => unreachable!(),
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

pub enum Item {
    Typedecl(Typedecl),
    Function(Function),
}
impl Item {
    pub fn function(&mut self) -> &mut Function {
        match self {
            Item::Function(f) => f,
            _ => panic!(),
        }
    }
    // pub fn name(&self) -> Ident {
    //     match self {
    //         Item::Typedecl(i) => i.name,
    //         Item::Function(i) => i.name,
    //     }
    // }
}

pub enum Interface {
    Typedecl(TypedeclInter),
    Function(FunctionInter),
}
impl Interface {
    pub fn into_func(&mut self) -> &mut FunctionInter {
        match self {
            Interface::Typedecl(_) => panic!("expected a function, but got a typedecl!"),
            Interface::Function(f) => f,
        }
    }
    pub fn name(&self) -> Ident {
        match self {
            Interface::Typedecl(t) => t.name,
            Interface::Function(f) => f.name,
        }
    }
}

pub struct Typedecl {
    // pub loc: Provenance,
    // pub name: Ident,
    // pub cases: Vec<Typecase>,
}
pub struct TypedeclInter {
    pub loc: Provenance,
    pub name: Ident,
    pub cases: Vec<Typecase>,
}

pub struct Typecase {
    pub loc: Provenance,
    pub name: Ident,
    pub binding: BindingId,
}

pub struct Function {
    // pub loc: Provenance,
    // pub name: Ident,
    // pub typ: Type,
    pub body: ExprId,
}
pub struct FunctionInter {
    pub loc: Provenance,
    pub name: Ident,
    pub typ: Type,
}

// impl Function {
//     pub fn fmt<'a>(
//         &'a self,
//         inter: &'a FunctionInter,
//         locals: &'a LocalVec,
//         typevec: &'a Typevec,
//         items: &'a Ivec<Id, Function>,
//     ) -> FunctionFmt<'a> {
//         FunctionFmt {
//             func: self,
//             typevec,
//             locals,
//             inter,
//             items,
//         }
//     }
// }

// pub struct FunctionFmt<'a> {
//     func: &'a Function,
//     inter: &'a FunctionInter,
//     locals: &'a LocalVec,
//     typevec: &'a Typevec,
//     items: &'a Ivec<Id, Function>,
// }
// impl std::fmt::Display for FunctionFmt<'_> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let name = self.inter.name.name.0;
//         let typ = self.inter.typ.fmt(self.typevec);
//         let body = self.locals[self.func.body].fmt(self.locals, self.typevec, self.items, 0);
//         write!(f, "func ({} ∈ {}) {}", name, typ, body)
//     }
// }

#[derive(Clone, Copy, Debug)]
pub struct Type {
    pub loc: Provenance,
    pub kind: TypeId,
}
impl Type {
    fn intern(typevec: &mut Typevec, kind: Typekind) -> TypeId {
        // something fucks this up, I'm guessing replace?
        // thread_local! {
        //     static STATE: std::cell::RefCell<BTreeMap<Typekind, TypeId>>  = const { std::cell::RefCell::new(BTreeMap::new()) }
        // }
        // STATE.with(|i| {
        //     let mut i = i.borrow_mut();
        //     if let Some(&d) = i.get(&kind) {
        //         return d;
        //     }
        //     let d = typevec.push(kind.clone());
        //     i.insert(kind, d);
        //     d
        // })
        typevec.push(kind)
    }

    pub fn fmt<'a>(&'a self, typevec: &'a Typevec) -> TypekindFmt<'a> {
        typevec[self.kind].fmt(typevec)
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
    // TODO: this would at some point be resolved to an
    // item id
    Recall(Istr),
    Var(usize),
    String,
    Bool,
    Int,
    Tup,
}
impl Typekind {
    pub fn fmt<'a>(&'a self, locals: &'a Typevec) -> TypekindFmt<'a> {
        TypekindFmt {
            kind: self,
            typevec: locals,
        }
    }
}

pub struct TypekindFmt<'a> {
    kind: &'a Typekind,
    typevec: &'a Typevec,
}
impl std::fmt::Display for TypekindFmt<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            Typekind::Func { args, ret } => write!(
                f,
                "func({}): {}",
                self.typevec[*args].fmt(self.typevec),
                self.typevec[*ret].fmt(self.typevec),
            ),
            Typekind::Call { args, func } => write!(
                f,
                "{}{}",
                self.typevec[*func].fmt(self.typevec),
                self.typevec[*args].fmt(self.typevec)
            ),
            Typekind::Bundle(items) => {
                write!(f, "(")?;
                if !items.is_empty() {
                    write!(f, "{}", self.typevec[items[0]].fmt(self.typevec))?;
                    for &i in &items[1..] {
                        write!(f, ", {}", self.typevec[i].fmt(self.typevec))?;
                    }
                }
                write!(f, ")")
            }
            Typekind::Recall(istr) => write!(f, "{}", istr.0),
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
// impl Expr {
//     pub fn fmt<'a>(
//         &'a self,
//         locals: &'a LocalVec,
//         typevec: &'a Typevec,
//         items: &'a Ivec<Id, Function>,
//         nesting: usize,
//     ) -> ExprFmt<'a> {
//         ExprFmt {
//             expr: self,
//             locals,
//             typevec,
//             items,
//             nesting,
//         }
//     }
// }

// pub struct ExprFmt<'a> {
//     expr: &'a Expr,
//     locals: &'a LocalVec,
//     typevec: &'a Typevec,
//     items: &'a Ivec<Id, Function>,
//     nesting: usize,
// }
// impl ExprFmt<'_> {
//     fn sub(&self, id: ExprId) -> Self {
//         self.locals[id].fmt(self.locals, self.typevec, self.items, self.nesting + 1)
//     }
// }
// impl std::fmt::Display for ExprFmt<'_> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let typ = self.expr.typ.fmt(self.typevec);
//         match self.expr.kind {
//             Exprkind::If { cond, smash, pass } => {
//                 let cond = self.sub(cond);
//                 let smash = self.sub(smash);
//                 let pass = self.sub(pass);
//                 write!(f, "(if {} {} else {} ∈ {})", cond, smash, pass, typ)
//             }
//             Exprkind::Call { func, args } => {
//                 let func = self.sub(func);
//                 let args = self.sub(args);
//                 write!(f, "({}{} ∈ {})", func, args, typ)
//             }
//             Exprkind::Builtin { builtin, args, .. } => {
//                 let args = self.sub(args);
//                 write!(f, "({}{} ∈ {})", builtin, args, typ)
//             }
//             Exprkind::Tuple(ref expr_ids) => {
//                 write!(f, "(")?;
//                 if let Some(&e1) = expr_ids.first() {
//                     let e1 = self.sub(e1);
//                     write!(f, "{}", e1)?;
//                     for &ex in &expr_ids[1..] {
//                         let ex = self.sub(ex);
//                         write!(f, ", {}", ex)?;
//                     }
//                 }
//                 write!(f, ")")
//             }
//             Exprkind::Loop(expr_id) => {
//                 let body = self.sub(expr_id);
//                 write!(f, "(loop {} ∈ {})", body, typ)
//             }
//             Exprkind::Bareblock(ref expr_ids) => {
//                 writeln!(f, "({{")?;
//                 for &e in expr_ids.as_ref() {
//                     for _ in 0..self.nesting + 1 {
//                         write!(f, "  ")?;
//                     }
//                     let e = self.sub(e);
//                     writeln!(f, "{}", e)?;
//                 }
//                 for _ in 0..self.nesting {
//                     write!(f, "  ")?;
//                 }
//                 write!(f, "}})")
//             }
//             Exprkind::Recall(binding_id) => {
//                 let binding = match binding_id {
//                     Ok(b) => {
//                         if let Bindkind::Name(n, _) = self.locals[b].kind {
//                             n.0
//                         } else {
//                             unreachable!()
//                         }
//                     }
//                     Err(i) => self.items[i].name.name.0,
//                 };
//                 write!(f, "({} ∈ {})", binding, typ)
//             }
//             Exprkind::Number(istr) | Exprkind::Bool(istr) => write!(f, "{}", istr.0),
//             Exprkind::String(istr) => write!(f, "{:?}", istr.0),
//             Exprkind::Arguments => write!(f, "Args"),
//             Exprkind::Poison => write!(f, "(!!)"),
//             Exprkind::Let(binding_id, expr_id) => {
//                 let binding = self.locals[binding_id].fmt(self.locals, self.typevec);
//                 let expr = self.sub(expr_id);
//                 write!(f, "let {} = {}", binding, expr)
//             }
//             Exprkind::Assign(binding_id, expr_id) => {
//                 let binding = self.locals[binding_id].fmt(self.locals, self.typevec);
//                 let expr = self.sub(expr_id);
//                 write!(f, "{} = {}", binding, expr)
//             }
//             Exprkind::Break { val, .. } => write!(f, "break {}", self.sub(val)),
//             Exprkind::Return(expr_id) => write!(f, "return {}", self.sub(expr_id)),
//             Exprkind::Const(expr_id) => write!(f, "{};", self.sub(expr_id)),
//         }
//     }
// }

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
// impl Binding {
//     pub fn fmt<'a>(&'a self, locals: &'a LocalVec, typevec: &'a Typevec) -> BindingFmt<'a> {
//         BindingFmt {
//             bind: self,
//             locals,
//             typevec,
//         }
//     }
// }

// pub struct BindingFmt<'a> {
//     bind: &'a Binding,
//     locals: &'a LocalVec,
//     typevec: &'a Typevec,
// }
// impl std::fmt::Display for BindingFmt<'_> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let typ = self.bind.typ.fmt(self.typevec);
//         match self.bind.kind {
//             Bindkind::Empty => write!(f, "(∅ ∈ {})", typ),
//             Bindkind::Name(istr, m) => {
//                 write!(f, "({} ∈ {}{})", istr.0, if m { "mut " } else { "" }, typ)
//             }
//             Bindkind::Tuple(ref binding_ids) => {
//                 write!(f, "(")?;
//                 if let Some(&b1) = binding_ids.first() {
//                     let b1 = self.locals[b1].fmt(self.locals, self.typevec);
//                     write!(f, "{}", b1)?;
//                     for &bx in &binding_ids[1..] {
//                         let bx = self.locals[bx].fmt(self.locals, self.typevec);
//                         write!(f, ", {}", bx)?;
//                     }
//                 }
//                 write!(f, ")")
//             }
//         }
//     }
// }

#[derive(Debug)]
pub enum Bindkind {
    Empty,
    Name(Istr, bool),
    Tuple(Rc<[BindingId]>),
}

fn resolve(ctx: &mut Tara, i: In) -> Out {
    let imports = ctx.preimport(preimport::In { m: i.m }).imports;
    let ast = ctx.parse(parse::In { m: i.m });
    let mut names = Svec::default();
    for i in imports.iter() {
        let items = ctx.resolve(In { m: i.target }).items;
        if let Some(decl) = i.head {
            for &id in items.iter() {
                if ctx.item_name(id).name != decl {
                    continue;
                }
                names.insert(ctx.item_name(id).name, Err(id));
                break;
            }
        } else {
            for &id in items.iter() {
                names.insert(ctx.item_name(id).name, Err(id));
            }
        }
    }
    let mut items = Vec::new();
    for i in ast.ast.items.iter() {
        let locals = LocalVec::default();
        let locals = ctx.uir_locals.push(locals);
        let (name, inter) = i.interface(locals, ctx);
        let id = locals;
        ctx.uir_interfaces.register(inter, id).expect("yaeh");
        items.push(id);
        names.insert(name, Err(id))
    }
    for (i, &locals) in ast.ast.items.iter().zip(items.iter()) {
        let item = names.excursion(|names| i.convert(locals, names, ctx));
        let id = locals;
        ctx.uir_items.register(item, id).expect("yaeh");
    }
    Out {
        items: items.into(),
    }
}

struct Tctx<'a> {
    id: Id,
    tara: &'a mut Tara,
    // modules: &'a Ivec<ModuleId, Module>,
    // items: &'a mut Ivec<Id, Item>,
    // locals: &'a mut LocalVec,
    // types: &'a mut Ivec<TypeId, Typekind>,
}
impl BindingCtxT for Tctx<'_> {
    fn modules(&self) -> &Ivec<ModuleId, Module> {
        &self.tara.modules
    }
    fn types(&mut self) -> &mut Typevec {
        &mut self.tara.uir_types
    }
    fn empty_type(&mut self) -> TypeId {
        let bundle = Type::intern(self.types(), Typekind::Bundle(Rc::new([])));
        let tup = Type::intern(self.types(), Typekind::Tup);
        Type::intern(
            self.types(),
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

impl parse::Item {
    fn interface(&self, locals: Id, tara: &mut Tara) -> (Istr, Interface) {
        match self {
            parse::Item::Function(f) => {
                let name = f.name;
                let loc = f.loc;
                let ret = f.ret.convert(&mut tara.uir_types);
                let arg = f.args.typing(tara);
                let typ = Type::func(&mut tara.uir_types, arg, ret, loc);
                (
                    name.name,
                    Interface::Function(FunctionInter { loc, typ, name }),
                )
            }
            parse::Item::Typedecl(t) => {
                let name = t.name;
                let loc = t.loc;
                let cases: Vec<_> = t.cases.iter().map(|c| c.convert(locals, tara)).collect();
                (
                    name.name,
                    Interface::Typedecl(TypedeclInter { loc, name, cases }),
                )
            }
        }
    }
    fn convert(
        &self,
        id: Id,
        names: &mut Svec<Istr, Result<BindingId, Id>>,
        ctx: &mut Tara,
    ) -> Item {
        match self {
            parse::Item::Function(i) => Item::Function(i.convert(id, names, ctx)),
            parse::Item::Typedecl(_) => Item::Typedecl(Typedecl {}),
            // ^ i.convert(
            //     id,
            //     &mut Tctx {
            //         id,
            //         tara: ctx,
            //         // modules: &ctx.modules,
            //         // items: &mut ctx.uir_items,
            //         // locals: &mut ctx.uir_locals[id],
            //         // types: &mut ctx.uir_types,
            //     },
            // ),
        }
    }
}

impl parse::Typecase {
    fn convert(&self, locals: Id, tara: &mut Tara) -> Typecase {
        let binding = self.binding.convert(false, &mut Tctx { id: locals, tara });
        Typecase {
            name: self.name,
            loc: self.loc,
            binding,
        }
    }
}

impl parse::Function {
    fn convert(
        &self,
        id: Id,
        names: &mut Svec<Istr, Result<BindingId, Id>>,
        tara: &mut Tara,
    ) -> Function {
        let bind = self.args.convert(
            false,
            &mut Ectx {
                loops: Vec::new(),
                names,
                tara,
                id,
            },
        );
        let block = self.body.convert(&mut Ectx {
            loops: Vec::new(),
            names,
            tara,
            id,
            // tvars: &mut tara.uir_tvars,
            // locals: &mut tara.uir_locals[id],
            // modules: &tara.modules,
            // items: &mut tara.uir_items,
            // types: &mut tara.uir_types,
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
    names: &'a mut Svec<Istr, Result<BindingId, Id>>,
    tara: &'a mut Tara,
    // tvars: &'a mut usize,
    // locals: &'a mut LocalVec,
    // modules: &'a Ivec<ModuleId, Module>,
    // items: &'a mut Ivec<Id, Item>,
    // types: &'a mut Ivec<TypeId, Typekind>,
}
impl Ectx<'_> {
    //     fn function(&mut self, i: Id, loc: Option<Provenance>) -> &mut Function {
    //         match &mut self.tara.uir_items[i] {
    //             Item::Function(f) => f,
    //             Item::Typedecl(t) => {
    //                 report(
    //                     &self.modules,
    //                     Message::error(
    //                         &format!(
    //                             "Expected a function, but the name '{}' refers to a type!",
    //                             t.name.name.0
    //                         ),
    //                         loc,
    //                     ),
    //                     &[Message::note("Type declared here:", Some(t.loc))],
    //                 );
    //                 std::process::exit(1);
    //             }
    //         }
    //     }
    fn new_typevar(&mut self) -> TypeId {
        let kind = Typekind::Var(self.tara.uir_tvars);
        self.tara.uir_tvars += 1;
        self.tara.uir_types.push(kind)
    }
}
impl Tara {
    pub fn item_type(&self, i: Id, loc: Option<Provenance>) -> Type {
        match self.uir_interfaces[i] {
            Interface::Typedecl(ref t) => {
                report(
                    &self.modules,
                    Message::error(
                        &format!(
                            "Expected a function, but the name '{}' refers to a type!",
                            t.name.name.0
                        ),
                        loc,
                    ),
                    &[Message::note("Type declared here:", Some(t.loc))],
                );
                std::process::exit(1);
            }
            Interface::Function(ref f) => f.typ,
        }
    }
}
impl BindingCtxT for Ectx<'_> {
    fn empty_type(&mut self) -> TypeId {
        self.new_typevar()
    }
    fn modules(&self) -> &Ivec<ModuleId, Module> {
        &self.tara.modules
    }
    fn types(&mut self) -> &mut Typevec {
        &mut self.tara.uir_types
    }
    fn no_type(&mut self) -> Option<TypeId> {
        Some(self.new_typevar())
    }
    fn locals(&mut self) -> &mut LocalVec {
        &mut self.tara.uir_locals[self.id]
    }
    fn push_name(&mut self, n: Istr, b: BindingId) {
        self.names.insert(n, Ok(b))
    }
}

pub trait BindingCtxT {
    fn modules(&self) -> &Ivec<ModuleId, Module>;
    fn types(&mut self) -> &mut Typevec;
    fn empty_type(&mut self) -> TypeId;
    fn no_type(&mut self) -> Option<TypeId>;
    fn locals(&mut self) -> &mut LocalVec;
    fn push_name(&mut self, _: Istr, _: BindingId);
}

pub trait BindingT {
    fn convert(&self, _: Provenance, mutable: bool, _: &mut dyn BindingCtxT) -> BindingId;
    fn typing(&self, _: Provenance, _: &mut Tara) -> Type;
}

impl parse::Binding {
    pub fn convert(&self, mutable: bool, ctx: &mut dyn BindingCtxT) -> BindingId {
        self.kind.convert(self.loc, mutable, ctx)
    }
    pub fn typing(&self, ctx: &mut Tara) -> Type {
        self.kind.typing(self.loc, ctx)
    }
}

impl BindingT for parse::binding::Empty {
    fn typing(&self, loc: Provenance, ctx: &mut Tara) -> Type {
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
    fn typing(&self, loc: Provenance, ctx: &mut Tara) -> Type {
        match self.1 {
            Some(ref t) => t.convert(&mut ctx.uir_types),
            None => {
                report(
                    &ctx.modules,
                    Message::error(
                        "Found an unannotated binding in an invalid context!",
                        Some(loc),
                    ),
                    &[Message::note("Add a type annotation!", None)],
                );
                std::process::exit(1);
            }
        }
    }
    fn convert(&self, loc: Provenance, mutable: bool, ctx: &mut dyn BindingCtxT) -> BindingId {
        let typ = match self.1 {
            Some(ref t) => t.convert(ctx.types()),
            None => match ctx.no_type() {
                Some(kind) => Type { kind, loc },
                None => {
                    report(
                        ctx.modules(),
                        Message::error(
                            "Found an unannotated binding in an invalid context!",
                            Some(loc),
                        ),
                        &[Message::note("Add a type annotation!", None)],
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
    fn typing(&self, loc: Provenance, ctx: &mut Tara) -> Type {
        let ts: Vec<_> = self.0.iter().map(|b| b.typing(ctx)).collect();
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
        let typ = Type::tup(ctx.types(), &ts, loc);
        ctx.locals().push_binding(Binding {
            kind: Bindkind::Tuple(bs.into()),
            typ,
            loc,
        })
    }
}

pub trait ExprT {
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
        // if func = recall(builtin) { func = builtin::...; }
        let func = self.func.convert(ctx);
        let args = self.args.convert(ctx);
        let typ = Type {
            kind: ctx.new_typevar(),
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
            kind: ctx.new_typevar(),
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
                kind: ctx.new_typevar(),
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
                    kind: ctx.new_typevar(),
                    loc,
                };
                return ctx.locals().push_expr(Expr {
                    kind: Exprkind::Builtin(builtin),
                    typ,
                    loc,
                });
            }
        }
        match ctx.names.find(&self.0) {
            Some(&id) => {
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
                report(
                    &ctx.tara.modules,
                    Message::error("Could not resolve the following name!", Some(loc)),
                    &[],
                );
                let typ = Type {
                    kind: ctx.new_typevar(),
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
            Some(Ok(bid)) => {
                let bid = *bid;
                let expr = self.1.convert(ctx);
                Exprkind::Assign(bid, expr)
            }
            Some(Err(gid)) => {
                report(
                    &ctx.tara.modules,
                    Message::error("The following name is not assignable!", Some(loc)),
                    &[Message::note(
                        &format!(
                            "'{}' refers to a global item!",
                            ctx.tara.item_name(*gid).name.0,
                        ),
                        Some(ctx.tara.item_name(*gid).loc),
                    )],
                );
                self.1.convert(ctx);
                Exprkind::Poison
            }
            None => {
                report(
                    &ctx.tara.modules,
                    Message::error("Could not resolve the following name!", Some(loc)),
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
                report(
                    &ctx.tara.modules,
                    Message::error("This break is not contained by any loops!", Some(loc)),
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
    fn convert(&self, typevec: &mut Typevec) -> Type {
        match &self.kind {
            parse::Typekind::Func { args, ret } => {
                let args = args.convert(typevec);
                let ret = ret.convert(typevec);
                Type::func(typevec, args, ret, self.loc)
            }
            parse::Typekind::Call { args, func } => {
                let func = func.convert(typevec);
                let args = args.convert(typevec);
                Type::call(typevec, func, args, self.loc)
            }
            parse::Typekind::Bundle(items) => {
                let items: Vec<_> = items.iter().map(|t| t.convert(typevec)).collect();
                Type::bundle(typevec, &items, self.loc)
            }
            parse::Typekind::Recall(istr) => {
                let kind = match *istr {
                    s if s == "int".into() => Typekind::Int,
                    s if s == "string".into() => Typekind::String,
                    s if s == "bool".into() => Typekind::Bool,
                    s if s == "tuple".into() => Typekind::Tup,
                    s => Typekind::Recall(s),
                };
                Type::simple(typevec, kind, self.loc)
            }
        }
    }
}
