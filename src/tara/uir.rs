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
MkIndexer!(pub LocalId, u32);
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct ExprId(LocalId);
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct BindingId(LocalId);
// #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
// pub struct TypekindId(LocalId);
MkIndexer!(pub TypeId, u32);
type Typevec = Ivec<TypeId, Typekind>;

impl TypeId {
    pub fn replace(&self, types: &mut Typevec, map: &std::collections::BTreeMap<usize, TypeId>) {
        match types[*self].clone() {
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
            Typekind::Var(v) => match map.get(&v) {
                Some(t) => types[*self] = types[*t].clone(),
                None => {}
            },
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

pub struct Function {
    pub loc: Provenance,
    pub name: Ident,
    pub typ: Type,
    pub body: ExprId,
    pub locals: LocalVec,
    tvar_count: usize,
}
impl Function {
    pub fn fmt<'a>(
        &'a self,
        typevec: &'a Typevec,
        items: &'a Ivec<Id, Function>,
    ) -> FunctionFmt<'a> {
        FunctionFmt {
            func: self,
            typevec,
            items,
        }
    }
}

pub struct FunctionFmt<'a> {
    func: &'a Function,
    typevec: &'a Typevec,
    items: &'a Ivec<Id, Function>,
}
impl std::fmt::Display for FunctionFmt<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = self.func.name.name.0;
        let typ = self.func.typ.fmt(self.typevec);
        let body =
            self.func.locals[self.func.body].fmt(&self.func.locals, self.typevec, self.items, 0);
        write!(f, "func ({} ∈ {}) {}", name, typ, body)
    }
}

#[derive(Clone, Debug)]
pub struct Type {
    pub loc: Provenance,
    pub kind: TypeId,
}
impl Type {
    pub fn fmt<'a>(&'a self, typevec: &'a Typevec) -> TypekindFmt<'a> {
        typevec[self.kind].fmt(typevec)
    }
    pub fn call(typevec: &mut Typevec, func: &Type, args: &Type, loc: Provenance) -> Type {
        let kind = typevec.push(Typekind::Call {
            func: func.kind,
            args: args.kind,
        });
        Type { loc, kind }
    }

    pub fn func(typevec: &mut Typevec, args: &Type, ret: &Type, loc: Provenance) -> Type {
        let kind = typevec.push(Typekind::Func {
            args: args.kind,
            ret: ret.kind,
        });
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
        let kind = typevec.push(kind);
        Type { loc, kind }
    }

    pub fn unit(typevec: &mut Typevec, loc: Provenance) -> Type {
        Self::tup(typevec, &[], loc)
    }

    pub fn tup(typevec: &mut Typevec, fields: &[Type], loc: Provenance) -> Type {
        let bundle = Type::bundle(typevec, fields, loc);
        let tup = typevec.push(Typekind::Tup);
        let kind = typevec.push(Typekind::Call {
            args: bundle.kind,
            func: tup,
        });
        Type { kind, loc }
    }

    pub fn bundle(typevec: &mut Typevec, fields: &[Type], loc: Provenance) -> Type {
        let kind: Vec<_> = fields.iter().map(|f| f.kind).collect();
        let kind = typevec.push(Typekind::Bundle(kind.into()));
        Type { loc, kind }
    }
}
impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}
impl Eq for Type {}

#[derive(Clone, Debug, PartialEq, Eq)]
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
                    for &i in &items[0..] {
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
impl Expr {
    pub fn fmt<'a>(
        &'a self,
        locals: &'a LocalVec,
        typevec: &'a Typevec,
        items: &'a Ivec<Id, Function>,
        nesting: usize,
    ) -> ExprFmt<'a> {
        ExprFmt {
            expr: self,
            locals,
            typevec,
            items,
            nesting,
        }
    }
}

pub struct ExprFmt<'a> {
    expr: &'a Expr,
    locals: &'a LocalVec,
    typevec: &'a Typevec,
    items: &'a Ivec<Id, Function>,
    nesting: usize,
}
impl ExprFmt<'_> {
    fn sub(&self, id: ExprId) -> Self {
        self.locals[id].fmt(self.locals, self.typevec, self.items, self.nesting + 1)
    }
}
impl std::fmt::Display for ExprFmt<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let typ = self.expr.typ.fmt(self.typevec);
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
                write!(f, "({{\n")?;
                for &e in expr_ids.as_ref() {
                    for _ in 0..self.nesting + 1 {
                        write!(f, "  ")?;
                    }
                    let e = self.sub(e);
                    write!(f, "{}\n", e)?;
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
                    Err(i) => self.items[i].name.name.0,
                };
                write!(f, "({} ∈ {})", binding, typ)
            }
            Exprkind::Number(istr) | Exprkind::Bool(istr) => write!(f, "{}", istr.0),
            Exprkind::String(istr) => write!(f, "{:?}", istr.0),
            Exprkind::Arguments => write!(f, "Args"),
            Exprkind::Poison => write!(f, "(!!)"),
            Exprkind::Let(binding_id, expr_id) => {
                let binding = self.locals[binding_id].fmt(self.locals, self.typevec);
                let expr = self.sub(expr_id);
                write!(f, "let {} = {}", binding, expr)
            }
            Exprkind::Assign(binding_id, expr_id) => {
                let binding = self.locals[binding_id].fmt(self.locals, self.typevec);
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

#[derive(Debug)]
pub struct Binding {
    pub loc: Provenance,
    pub typ: Type,
    pub kind: Bindkind,
}
impl Binding {
    pub fn fmt<'a>(&'a self, locals: &'a LocalVec, typevec: &'a Typevec) -> BindingFmt<'a> {
        BindingFmt {
            bind: self,
            locals,
            typevec,
        }
    }
}

pub struct BindingFmt<'a> {
    bind: &'a Binding,
    locals: &'a LocalVec,
    typevec: &'a Typevec,
}
impl std::fmt::Display for BindingFmt<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let typ = self.bind.typ.fmt(self.typevec);
        match self.bind.kind {
            Bindkind::Empty => write!(f, "(∅ ∈ {})", typ),
            Bindkind::Name(istr, m) => {
                write!(f, "({} ∈ {}{})", istr.0, if m { "mut " } else { "" }, typ)
            }
            Bindkind::Tuple(ref binding_ids) => {
                write!(f, "(")?;
                if let Some(&b1) = binding_ids.first() {
                    let b1 = self.locals[b1].fmt(self.locals, self.typevec);
                    write!(f, "{}", b1)?;
                    for &bx in &binding_ids[1..] {
                        let bx = self.locals[bx].fmt(self.locals, self.typevec);
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

fn resolve(ctx: &mut Tara, i: In) -> Out {
    let imports = ctx.preimport(preimport::In { m: i.m }).imports;
    let ast = ctx.parse(parse::In { m: i.m });
    let mut names = Svec::default();
    for i in imports.iter() {
        let items = ctx.resolve(In { m: i.target }).items;
        if let Some(decl) = i.head {
            for &id in items.iter() {
                if ctx.uir_items[id].name.name == decl {
                    continue;
                }
                names.insert(ctx.uir_items[id].name.name, Err(id));
                break;
            }
        } else {
            for &id in items.iter() {
                names.insert(ctx.uir_items[id].name.name, Err(id));
            }
        }
    }
    let mut items = Vec::new();
    for f in ast.ast.funcs.iter() {
        let locals = LocalVec::default();
        let typ = {
            let args = binding_type(&mut ctx.uir_types, &mut 0, &f.args);
            let ret = types(&mut ctx.uir_types, &f.ret);
            Type::func(&mut ctx.uir_types, &args, &ret, f.loc)
        };
        let body = locals.promise_expr();
        // ^ this is just a placeholder, it will be replaced in self.functions
        let id = ctx.uir_items.push(Function {
            tvar_count: 0,
            loc: f.loc,
            name: f.name,
            typ,
            body,
            locals,
        });
        items.push(id);
        names.insert(f.name.name, Err(id));
    }
    let names_start = names.len();
    for (f, &id) in ast.ast.funcs.iter().zip(items.iter()) {
        Context {
            modules: &mut ctx.modules,
            items: &mut ctx.uir_items,
            typevec: &mut ctx.uir_types,
            func: id,
            names: &mut names,
            loops: Vec::new(),
        }
        .functions(f);
        names.truncate(names_start);
    }
    Out {
        items: items.into(),
    }
}

struct Context<'a> {
    func: Id,
    modules: &'a Ivec<ModuleId, Module>,
    items: &'a mut Ivec<Id, Function>,
    typevec: &'a mut Typevec,
    names: &'a mut Svec<Istr, Result<BindingId, Id>>,
    loops: Vec<ExprId>,
}
impl Context<'_> {
    fn new_typevar(&mut self, loc: Provenance) -> Type {
        new_typevar(self.typevec, &mut self.items[self.func].tvar_count, loc)
    }
    fn item(&mut self) -> &mut Function {
        &mut self.items[self.func]
    }

    fn functions(&mut self, f: &parse::Function) {
        let bind = self.bindings(&f.args, false);
        let block = self.expressions(&f.body);
        let args = {
            let item = &mut self.items[self.func];
            Expr {
                loc: f.args.loc(),
                typ: item.typ.argument(&mut self.typevec).unwrap(),
                kind: Exprkind::Arguments,
            }
        };
        let args = self.item().locals.push_expr(args);
        let bind = Expr {
            loc: f.args.loc(),
            typ: Type::unit(self.typevec, f.args.loc()),
            kind: Exprkind::Let(bind, args),
        };
        let bind = self.item().locals.push_expr(bind);
        let body = Expr {
            loc: f.args.loc(),
            typ: self.item().locals[block].typ.clone(),
            kind: Exprkind::Bareblock(Rc::new([bind, block])),
        };
        let body = self.item().locals.push_expr(body);
        self.item().body = body;
    }

    fn expressions(&mut self, e: &parse::Expr) -> ExprId {
        let (typ, kind) = match e.kind {
            parse::Exprkind::If(ref exprs) => {
                let [cond, smash, pass] = exprs.as_ref();
                let cond = self.expressions(cond);
                let smash = self.expressions(smash);
                let pass = self.expressions(pass);
                (
                    self.item().locals[smash].typ.clone(),
                    Exprkind::If { cond, smash, pass },
                )
            }
            parse::Exprkind::Call(ref exprs) => {
                let [func, args] = exprs.as_ref();
                let func = self.expressions(func);
                let args = self.expressions(args);
                (self.new_typevar(e.loc), Exprkind::Call { func, args })
            }
            parse::Exprkind::Tuple(ref exprs) => {
                let (types, exprs): (Vec<_>, Vec<_>) = exprs
                    .iter()
                    .map(|e| {
                        let e = self.expressions(e);
                        (self.item().locals[e].typ.clone(), e)
                    })
                    .unzip();
                (
                    Type::tup(&mut self.typevec, &types, e.loc),
                    Exprkind::Tuple(exprs.into()),
                )
            }
            parse::Exprkind::Loop(ref expr) => {
                // 'reserve' a local slot
                let slot = Expr {
                    loc: e.loc,
                    typ: Type::unit(&mut self.typevec, e.loc),
                    kind: Exprkind::default(),
                };
                let slot = self.item().locals.push_expr(slot);
                self.loops.push(slot);
                let expr = self.expressions(expr);
                self.loops.pop();
                let typ = self.new_typevar(e.loc);
                let kind = Exprkind::Loop(expr);
                self.item().locals[slot] = Expr {
                    loc: e.loc,
                    typ,
                    kind,
                };
                return slot;
            }
            parse::Exprkind::Bareblock(ref exprs) => {
                let exprs: Vec<_> = exprs.iter().map(|e| self.expressions(e)).collect();
                let typ = exprs
                    .last()
                    .map(|&id| self.item().locals[id].typ.clone())
                    .unwrap_or_else(|| self.new_typevar(e.loc));
                (typ, Exprkind::Bareblock(exprs.into()))
            }
            parse::Exprkind::Recall(istr) => match self.names.find(&istr) {
                Some(&id) => {
                    let typ = id
                        .map_err(|e| self.items[e].typ.clone())
                        .map(|l| self.item().locals[l].typ.clone())
                        .unwrap_or_else(|e| e);
                    (typ, Exprkind::Recall(id))
                }
                _ => {
                    report(
                        self.modules,
                        Message::error("Could not resolve the following name!", Some(e.loc)),
                        &[],
                    );
                    (self.new_typevar(e.loc), Exprkind::Poison)
                }
            },
            parse::Exprkind::Number(istr) => (
                Type::simple(&mut self.typevec, Typekind::Int, e.loc),
                Exprkind::Number(istr),
            ),
            parse::Exprkind::String(istr) => (
                Type::simple(&mut self.typevec, Typekind::String, e.loc),
                Exprkind::String(istr),
            ),
            parse::Exprkind::Bool(istr) => (
                Type::simple(&mut self.typevec, Typekind::Bool, e.loc),
                Exprkind::Bool(istr),
            ),
            parse::Exprkind::Let(ref binding, ref expr) => {
                let binding = self.bindings(binding, false);
                let expr = self.expressions(expr);
                (
                    Type::unit(self.typevec, e.loc),
                    Exprkind::Let(binding, expr),
                )
            }
            parse::Exprkind::Mut(ref binding, ref expr) => {
                let binding = self.bindings(binding, true);
                let expr = self.expressions(expr);
                (
                    Type::unit(self.typevec, e.loc),
                    Exprkind::Let(binding, expr),
                )
            }
            parse::Exprkind::Assign(istr, ref expr) => {
                // TODO: reports here should get at the binding provenance, not the whole expr
                match self.names.find(&istr.name) {
                    Some(Ok(bid)) => {
                        let bid = *bid;
                        let expr = self.expressions(expr);
                        (
                            Type::unit(&mut self.typevec, e.loc),
                            Exprkind::Assign(bid, expr),
                        )
                    }
                    Some(Err(gid)) => {
                        report(
                            self.modules,
                            Message::error("The following name is not assignable!", Some(istr.loc)),
                            // TODO: point to the name instead of the whole item
                            &[Message::note(
                                &format!(
                                    "'{}' refers to a global item!",
                                    self.items[*gid].name.name.0
                                ),
                                Some(self.items[*gid].name.loc),
                            )],
                        );
                        self.expressions(expr);
                        (Type::unit(&mut self.typevec, e.loc), Exprkind::Poison)
                    }
                    None => {
                        report(
                            self.modules,
                            Message::error("Could not resolve the following name!", Some(e.loc)),
                            &[],
                        );
                        self.expressions(expr);
                        (Type::unit(&mut self.typevec, e.loc), Exprkind::Poison)
                    }
                }
            }
            parse::Exprkind::Break(ref expr) => match self.loops.last() {
                Some(&target) => {
                    let val = if let Some(expr) = expr {
                        self.expressions(expr)
                    } else {
                        let val = Expr {
                            loc: e.loc,
                            typ: Type::unit(&mut self.typevec, e.loc),
                            kind: Exprkind::default(),
                        };
                        self.item().locals.push_expr(val)
                    };
                    (
                        Type::unit(&mut self.typevec, e.loc),
                        Exprkind::Break { val, target },
                    )
                }
                None => {
                    report(
                        self.modules,
                        Message::error("This break is not contained by any loops!", Some(e.loc)),
                        &[],
                    );
                    (Type::unit(&mut self.typevec, e.loc), Exprkind::Poison)
                }
            },
            parse::Exprkind::Return(None) => {
                let expr = Expr {
                    loc: e.loc,
                    typ: Type::unit(&mut self.typevec, e.loc),
                    kind: Exprkind::default(),
                };
                let expr = self.item().locals.push_expr(expr);
                (Type::unit(&mut self.typevec, e.loc), Exprkind::Return(expr))
            }
            parse::Exprkind::Return(Some(ref expr)) => (
                Type::unit(&mut self.typevec, e.loc),
                Exprkind::Return(self.expressions(expr)),
            ),
            parse::Exprkind::Const(ref expr) => (
                Type::unit(&mut self.typevec, e.loc),
                Exprkind::Const(self.expressions(expr)),
            ),
        };
        self.item().locals.push_expr(Expr {
            loc: e.loc,
            typ,
            kind,
        })
    }

    fn bindings(&mut self, b: &parse::Binding, mutable: bool) -> BindingId {
        let (name, binding) = match b {
            parse::Binding::Empty(loc) => (
                None,
                Binding {
                    kind: Bindkind::Empty,
                    typ: self.new_typevar(*loc),
                    loc: *loc,
                },
            ),
            parse::Binding::Name(loc, istr, Some(t)) => (
                Some(*istr),
                Binding {
                    kind: Bindkind::Name(*istr, mutable),
                    typ: self.types(t),
                    loc: *loc,
                },
            ),
            parse::Binding::Name(loc, istr, None) => (
                Some(*istr),
                Binding {
                    kind: Bindkind::Name(*istr, mutable),
                    typ: self.new_typevar(*loc),
                    loc: *loc,
                },
            ),
            parse::Binding::Tuple(loc, bindings) => {
                let (ts, bs): (Vec<_>, Vec<_>) = bindings
                    .iter()
                    .map(|b| {
                        let b = self.bindings(b, mutable);
                        (self.item().locals[b].typ.clone(), b)
                    })
                    .unzip();
                (
                    None,
                    Binding {
                        kind: Bindkind::Tuple(bs.into()),
                        typ: Type::tup(&mut self.typevec, &ts, *loc),
                        loc: *loc,
                    },
                )
            }
        };
        let bid = self.item().locals.push_binding(binding);
        if let Some(name) = name {
            self.names.insert(name, Ok(bid));
        }
        bid
    }

    fn types(&mut self, t: &parse::Type) -> Type {
        types(&mut self.typevec, t)
    }
}

fn binding_type(typevec: &mut Typevec, counter: &mut usize, b: &parse::Binding) -> Type {
    match b {
        parse::Binding::Empty(loc) => Type::unit(typevec, *loc),
        parse::Binding::Name(_, _, Some(t)) => types(typevec, t),
        parse::Binding::Name(loc, _, None) => new_typevar(typevec, counter, *loc),
        parse::Binding::Tuple(loc, bindings) => {
            let fields: Vec<_> = bindings
                .iter()
                .map(|b| binding_type(typevec, counter, b))
                .collect();
            Type::tup(typevec, &fields, *loc)
        }
    }
}

fn new_typevar(typevec: &mut Typevec, counter: &mut usize, loc: Provenance) -> Type {
    let kind = Typekind::Var(*counter);
    *counter += 1;
    Type::simple(typevec, kind, loc)
}

fn types(typevec: &mut Typevec, t: &parse::Type) -> Type {
    match &t.kind {
        parse::Typekind::Func { args, ret } => {
            let args = types(typevec, args);
            let ret = types(typevec, ret);
            Type::func(typevec, &args, &ret, t.loc)
        }
        parse::Typekind::Call { args, func } => {
            let func = types(typevec, func);
            let args = types(typevec, args);
            Type::call(typevec, &func, &args, t.loc)
        }
        parse::Typekind::Bundle(items) => {
            let items: Vec<_> = items.iter().map(|t| types(typevec, t)).collect();
            Type::bundle(typevec, &items, t.loc)
        }
        parse::Typekind::Recall(istr) => {
            let kind = match *istr {
                s if s == "int".into() => Typekind::Int,
                s if s == "string".into() => Typekind::String,
                s if s == "bool".into() => Typekind::Bool,
                s if s == "tuple".into() => Typekind::Tup,
                s => Typekind::Recall(s),
            };
            Type::simple(typevec, kind, t.loc)
        }
    }
}
