use std::{fmt::Display, rc::Rc};

use super::{preimport, ModuleId, Tara};
use crate::{
    ansi::Style,
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
                let data = Context::default().resolve(self, i);
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
    pub name: Istr,
    pub typ: Type,
    pub body: ExprId,
    pub locals: LocalVec,
}

#[derive(Clone, Debug)]
pub struct Type {
    pub loc: Provenance,
    pub kind: Typekind,
}
impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}
impl Type {
    pub fn replace(&mut self, t: usize, by: &Type) {
        match &mut self.kind {
            Typekind::Func { args, ret } => {
                args.replace(t, by);
                ret.replace(t, by);
            }
            Typekind::Call { args, func } => {
                args.replace(t, by);
                func.replace(t, by);
            }
            Typekind::Bundle(items) => {
                for i in items.iter_mut() {
                    i.replace(t, by);
                }
            }
            Typekind::Var(v) if t == *v => {
                self.kind = by.kind.clone();
            }
            _ => {}
        }
    }
    pub fn return_type(&self) -> Option<&Type> {
        match self.kind {
            Typekind::Func { ref ret, .. } => Some(ret),
            _ => None,
        }
    }

    pub fn unit(loc: Provenance) -> Type {
        Self::tup(Vec::new(), loc)
    }

    pub fn tup(fields: Vec<Type>, loc: Provenance) -> Type {
        Type {
            kind: Typekind::Call {
                args: Box::new(Type {
                    kind: Typekind::Bundle(fields),
                    loc,
                }),
                func: Box::new(Type {
                    kind: Typekind::Tup,
                    loc,
                }),
            },
            loc,
        }
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
    Func { args: Box<Type>, ret: Box<Type> },
    Call { args: Box<Type>, func: Box<Type> },
    Bundle(Vec<Type>),
    // TODO: this would at some point be resolved to an
    // item id
    Recall(Istr),
    Var(usize),
    String,
    Bool,
    Int,
    Tup,
}
impl Default for Typekind {
    fn default() -> Self {
        Typekind::Bundle(Vec::new())
    }
}
impl Display for Typekind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Typekind::Func { args, ret } => write!(f, "func{}: {}", args, ret),
            Typekind::Call { args, func } => write!(f, "{}{}", func, args),
            Typekind::Bundle(items) => {
                write!(f, "(")?;
                if !items.is_empty() {
                    write!(f, "{}", &items[0])?;
                    for i in &items[0..] {
                        write!(f, ", {}", i)?;
                    }
                }
                write!(f, ")")
            }
            Typekind::Recall(istr) => write!(f, "{}", istr.0),
            Typekind::Var(i) => write!(f, "?{}", i),
            Typekind::String => write!(f, "string"),
            Typekind::Bool => write!(f, "bool"),
            Typekind::Int => write!(f, "int"),
            Typekind::Tup => write!(f, "Tuple"),
        }
    }
}

#[derive(Debug)]
pub enum Locals {
    Expr(Expr),
    Binding(Binding),
}
impl Locals {
    pub fn typ(&self) -> &Type {
        match self {
            Self::Expr(e) => &e.typ,
            Self::Binding(b) => &b.typ,
        }
    }
}

#[derive(Debug)]
pub struct Expr {
    pub loc: Provenance,
    pub typ: Type,
    pub kind: Exprkind,
}
#[derive(Debug)]
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
    Tuple(Vec<ExprId>),
    Loop(ExprId),
    Bareblock(Vec<ExprId>),
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
        Exprkind::Tuple(Vec::new())
    }
}

#[derive(Debug)]
pub struct Binding {
    pub loc: Provenance,
    pub typ: Type,
    pub kind: Bindkind,
}
#[derive(Debug)]
pub enum Bindkind {
    Empty,
    Name(Istr, bool),
    Tuple(Vec<BindingId>),
}

#[derive(Default)]
struct Context {
    names: Svec<Istr, Result<BindingId, Id>>,
    loops: Vec<ExprId>,
    tvar_count: usize,
}
impl Context {
    fn new_typevar(&mut self, loc: Provenance) -> Type {
        let kind = Typekind::Var(self.tvar_count);
        self.tvar_count += 1;
        Type { kind, loc }
    }

    fn resolve(&mut self, ctx: &mut Tara, i: In) -> Out {
        let imports = ctx.preimport(preimport::In { m: i.m }).imports;
        let ast = ctx.parse(parse::In { m: i.m });
        for i in imports.iter() {
            let items = ctx.resolve(In { m: i.target }).items;
            if let Some(decl) = i.head {
                for &id in items.iter() {
                    if ctx.uir_items[id].name == decl {
                        continue;
                    }
                    self.names.insert(ctx.uir_items[id].name, Err(id));
                    break;
                }
            } else {
                for &id in items.iter() {
                    self.names.insert(ctx.uir_items[id].name, Err(id));
                }
            }
        }
        let mut items = Vec::new();
        for f in ast.ast.funcs.iter() {
            let locals = LocalVec::default();
            // the locals will be replaced anyway
            // so we just create some default values
            // to avoid option wrapping for no good reason
            let body = locals.promise_expr();
            let id = ctx.uir_items.push(Function {
                loc: f.loc,
                name: f.name,
                typ: Type {
                    loc: f.loc,
                    kind: Typekind::Func {
                        args: Box::new(self.binding_type(&f.args)),
                        ret: Box::new(Self::types(&f.ret)),
                    },
                },
                body,
                locals,
            });
            items.push(id);
            self.names.insert(f.name, Err(id));
        }
        for (f, &id) in ast.ast.funcs.iter().zip(items.iter()) {
            self.functions(ctx, f, id);
        }
        Out {
            items: items.into(),
        }
    }

    fn functions(&mut self, ctx: &mut Tara, f: &parse::Function, out: Id) {
        let mut locals = LocalVec::default();
        let bind = self.bindings(&mut locals, &f.args, false);
        let block = self.expressions(&mut locals, ctx, &f.body);
        let args = locals.push_expr(Expr {
            loc: f.args.loc(),
            typ: ctx.uir_items[out].typ.return_type().unwrap().clone(),
            kind: Exprkind::Arguments,
        });
        let args = locals.push_expr(Expr {
            loc: f.args.loc(),
            typ: Type::unit(f.args.loc()),
            kind: Exprkind::Let(bind, args),
        });
        let body = locals.push_expr(Expr {
            loc: f.args.loc(),
            typ: locals[block].typ.clone(),
            kind: Exprkind::Bareblock(vec![args, block]),
        });
        ctx.uir_items[out].locals = locals;
        ctx.uir_items[out].body = body;
    }

    fn expressions(&mut self, locals: &mut LocalVec, ctx: &mut Tara, e: &parse::Expr) -> ExprId {
        let (typ, kind) = match e.kind {
            parse::Exprkind::If(ref exprs) => {
                let [cond, smash, pass] = exprs.as_ref();
                let cond = self.expressions(locals, ctx, cond);
                let smash = self.expressions(locals, ctx, smash);
                let pass = self.expressions(locals, ctx, pass);
                (
                    locals[smash].typ.clone(),
                    Exprkind::If { cond, smash, pass },
                )
            }
            parse::Exprkind::Call(ref exprs) => {
                let [func, args] = exprs.as_ref();
                let func = self.expressions(locals, ctx, func);
                let args = self.expressions(locals, ctx, args);
                (self.new_typevar(e.loc), Exprkind::Call { func, args })
            }
            parse::Exprkind::Tuple(ref exprs) => {
                let (types, exprs) = exprs
                    .iter()
                    .map(|e| {
                        let id = self.expressions(locals, ctx, e);
                        (locals[id].typ.clone(), id)
                    })
                    .unzip();
                (Type::tup(types, e.loc), Exprkind::Tuple(exprs))
            }
            parse::Exprkind::Loop(ref expr) => {
                // 'reserve' a local slot
                let id = locals.push_expr(Expr {
                    loc: e.loc,
                    typ: Type::unit(e.loc),
                    kind: Exprkind::default(),
                });
                self.loops.push(id);
                let expr = self.expressions(locals, ctx, expr);
                self.loops.pop();
                let typ = self.new_typevar(e.loc);
                let kind = Exprkind::Loop(expr);
                locals[id] = Expr {
                    loc: e.loc,
                    typ,
                    kind,
                };
                return id;
            }
            parse::Exprkind::Bareblock(ref exprs) => {
                let exprs: Vec<_> = exprs
                    .iter()
                    .map(|e| self.expressions(locals, ctx, e))
                    .collect();
                let typ = exprs
                    .last()
                    .map_or_else(|| self.new_typevar(e.loc), |&id| locals[id].typ.clone());
                (typ, Exprkind::Bareblock(exprs))
            }
            parse::Exprkind::Recall(istr) => match self.names.find(&istr) {
                Some(&id) => {
                    let typ =
                        id.map_or_else(|e| ctx.uir_items[e].typ.clone(), |l| locals[l].typ.clone());
                    (typ, Exprkind::Recall(id))
                }
                _ => {
                    e.loc.report::<&str>(
                        ctx,
                        Style::red() | Style::underline(),
                        Style::red().apply("Error"),
                        "Could not resolve the following name!",
                        [].into_iter(),
                    );
                    (self.new_typevar(e.loc), Exprkind::Poison)
                }
            },
            parse::Exprkind::Number(istr) => (
                Type {
                    loc: e.loc,
                    kind: Typekind::Int,
                },
                Exprkind::Number(istr),
            ),
            parse::Exprkind::String(istr) => (
                Type {
                    loc: e.loc,
                    kind: Typekind::String,
                },
                Exprkind::String(istr),
            ),
            parse::Exprkind::Bool(istr) => (
                Type {
                    loc: e.loc,
                    kind: Typekind::Bool,
                },
                Exprkind::Bool(istr),
            ),
            parse::Exprkind::Let(ref binding, ref expr) => {
                let binding = self.bindings(locals, binding, false);
                let expr = self.expressions(locals, ctx, expr);
                (Type::unit(e.loc), Exprkind::Let(binding, expr))
            }
            parse::Exprkind::Mut(ref binding, ref expr) => {
                let binding = self.bindings(locals, binding, true);
                let expr = self.expressions(locals, ctx, expr);
                (Type::unit(e.loc), Exprkind::Let(binding, expr))
            }
            parse::Exprkind::Assign(istr, ref expr) => {
                // TODO: reports here should get at the binding provenance, not the whole expr
                match self.names.find(&istr) {
                    Some(Ok(bid)) => {
                        let bid = *bid;
                        let expr = self.expressions(locals, ctx, expr);
                        (Type::unit(e.loc), Exprkind::Assign(bid, expr))
                    }
                    Some(Err(gid)) => {
                        report(
                            ctx,
                            Message::error("The following name is not assignable!", Some(e.loc)),
                            // TODO: point to the name instead of the whole item
                            &[Message::note(
                                &format!(
                                    "'{}' refers to a global item!",
                                    ctx.uir_items[*gid].name.0
                                ),
                                Some(ctx.uir_items[*gid].loc),
                            )],
                        );
                        self.expressions(locals, ctx, expr);
                        (Type::unit(e.loc), Exprkind::Poison)
                    }
                    None => {
                        report(
                            ctx,
                            Message::error("Could not resolve the following name!", Some(e.loc)),
                            &[],
                        );
                        self.expressions(locals, ctx, expr);
                        (Type::unit(e.loc), Exprkind::Poison)
                    }
                }
            }
            parse::Exprkind::Break(ref expr) => match self.loops.last() {
                Some(&target) => {
                    let val = if let Some(expr) = expr {
                        self.expressions(locals, ctx, expr)
                    } else {
                        locals.push_expr(Expr {
                            loc: e.loc,
                            typ: Type::unit(e.loc),
                            kind: Exprkind::default(),
                        })
                    };
                    (Type::unit(e.loc), Exprkind::Break { val, target })
                }
                None => {
                    e.loc.report::<&str>(
                        ctx,
                        Provenance::RED_PTR,
                        Provenance::ERROR,
                        "This break is not contained by any loops!",
                        [].into_iter(),
                    );
                    (Type::unit(e.loc), Exprkind::Poison)
                }
            },
            parse::Exprkind::Return(None) => {
                let expr = locals.push_expr(Expr {
                    loc: e.loc,
                    typ: Type::unit(e.loc),
                    kind: Exprkind::default(),
                });
                (Type::unit(e.loc), Exprkind::Return(expr))
            }
            parse::Exprkind::Return(Some(ref expr)) => (
                Type::unit(e.loc),
                Exprkind::Return(self.expressions(locals, ctx, expr)),
            ),
            parse::Exprkind::Const(ref expr) => (
                Type::unit(e.loc),
                Exprkind::Const(self.expressions(locals, ctx, expr)),
            ),
        };
        locals.push_expr(Expr {
            loc: e.loc,
            typ,
            kind,
        })
    }

    fn bindings(&mut self, locals: &mut LocalVec, b: &parse::Binding, mutable: bool) -> BindingId {
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
                    typ: Self::types(t),
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
                let (ts, bs) = bindings
                    .iter()
                    .map(|b| {
                        let b = self.bindings(locals, b, mutable);
                        (locals[b].typ.clone(), b)
                    })
                    .unzip();
                (
                    None,
                    Binding {
                        kind: Bindkind::Tuple(bs),
                        typ: Type::tup(ts, *loc),
                        loc: *loc,
                    },
                )
            }
        };
        let bid = locals.push_binding(binding);
        if let Some(name) = name {
            self.names.insert(name, Ok(bid));
        }
        bid
    }

    fn binding_type(&mut self, b: &parse::Binding) -> Type {
        match b {
            parse::Binding::Empty(loc) => Type::unit(*loc),
            parse::Binding::Name(_, _, Some(t)) => Self::types(t),
            parse::Binding::Name(loc, _, None) => self.new_typevar(*loc),
            parse::Binding::Tuple(loc, bindings) => Type::tup(
                bindings.iter().map(|b| self.binding_type(b)).collect(),
                *loc,
            ),
        }
    }

    fn types(t: &parse::Type) -> Type {
        Type {
            loc: t.loc,
            kind: match &t.kind {
                parse::Typekind::Func { args, ret } => {
                    let args = Box::new(Self::types(args));
                    let ret = Box::new(Self::types(ret));
                    Typekind::Func { args, ret }
                }
                parse::Typekind::Call { args, func } => {
                    let func = Box::new(Self::types(func));
                    let args = Box::new(Self::types(args));
                    Typekind::Call { func, args }
                }
                parse::Typekind::Bundle(items) => {
                    let items = items.iter().map(Self::types).collect();
                    Typekind::Bundle(items)
                }
                parse::Typekind::Recall(istr) if *istr == "int".into() => Typekind::Int,
                parse::Typekind::Recall(istr) if *istr == "string".into() => Typekind::String,
                parse::Typekind::Recall(istr) if *istr == "bool".into() => Typekind::Bool,
                parse::Typekind::Recall(istr) => Typekind::Recall(*istr),
            },
        }
    }
}
