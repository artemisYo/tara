use either::Either::{self, Left, Right};

use super::{
    preimport,
    quir::{self, expr, Binding, BindingId, Expr, ExprId, LocalvecId, Type, TypeId},
    ModuleId, Tara,
};
use crate::{
    gen_query, message,
    misc::{CheapClone, Istr, Svec},
    quir::binding,
    Provenance,
};

use std::collections::BTreeMap as Map;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub struct In {
    pub m: ModuleId,
}

#[derive(Clone, Copy)]
pub struct Out {}
impl CheapClone for Out {}

gen_query!(resolve);
fn resolve(tara: &mut Tara, i: In) -> Out {
    let imports = tara.preimport(preimport::In { m: i.m }).imports;
    let mut globals = Map::new();
    for i in imports.iter() {
        let target = tara.quir(quir::In { m: i.target }).namespace;
        dbg!(&i);
        dbg!(&tara.quir_items[target].items);
        match i.head {
            Some(name) => {
                if let Some(id) = tara.quir_items[target].items.get(&name).copied() {
                    globals.insert(name, id);
                } else {
                    tara.report(
                        message!(
                            error @ i.loc
                                => "Could not resolve the import statement to item '{}'", name.0
                        ),
                        &[],
                    );
                }
            }
            None => {
                for (&name, &id) in tara.quir_items[target].items.iter() {
                    globals.insert(name, id);
                }
            }
        }
    }
    let quir = tara.quir(quir::In { m: i.m });
    for (&name, &id) in tara.quir_items[quir.namespace].items.iter() {
        globals.insert(name, id);
    }
    for &id in tara.quir_items[quir.namespace].items.cheap().values() {
        id.resolve(&globals, tara);
    }
    Out {}
}

impl quir::Id {
    fn index(self, name: Istr, tara: &Tara) -> Result<Option<quir::Id>, ()> {
        match tara.quir_items[self] {
            quir::Item::Typecase(_) | quir::Item::Function(_) => Err(()),
            quir::Item::Typedecl(ref t) => {
                for &c in t.cases.iter() {
                    if tara.quir_items[c].name.name == name {
                        return Ok(Some(c.into()));
                    }
                }
                Ok(None)
            }
            quir::Item::Namespace(ref n) => Ok(n.items.get(&name).copied()),
        }
    }
    fn resolve(self, globals: &Map<Istr, quir::Id>, tara: &mut Tara) {
        match tara.quir_items[self] {
            quir::Item::Namespace(ref n) => {
                for &i in n.items.cheap().values() {
                    i.resolve(globals, tara);
                }
            }
            quir::Item::Typedecl(ref t) => {
                for &c in t.cases.cheap().iter() {
                    quir::Id::from(c).resolve(globals, tara);
                }
            }
            quir::Item::Typecase(ref t) => {
                let typ = t.typ;
                let binding = t.binding;
                let id = self.into_localvec();
                typ.resolve(globals, tara);
                binding.resolve(&mut Ctx::new(id, globals, tara));
            }
            quir::Item::Function(ref f) => {
                let typ = f.typ;
                let body = f.body;
                let id = self.into_localvec();
                typ.resolve(globals, tara);
                body.resolve(&mut Ctx::new(id, globals, tara));
            }
        }
    }
}

impl Type {
    fn resolve(&self, globals: &Map<Istr, quir::Id>, tara: &mut Tara) {
        self.kind.resolve(self.loc, globals, tara);
    }
}
impl TypeId {
    fn resolve(self, loc: Provenance, globals: &Map<Istr, quir::Id>, tara: &mut Tara) {
        match tara.quir_types[self] {
            quir::Typekind::Func { args, ret } => {
                args.resolve(loc, globals, tara);
                ret.resolve(loc, globals, tara);
            }
            quir::Typekind::Call { args, func } => {
                args.resolve(loc, globals, tara);
                func.resolve(loc, globals, tara);
            }
            quir::Typekind::Bundle(ref ids) => {
                for &i in ids.cheap().iter() {
                    i.resolve(loc, globals, tara);
                }
            }
            quir::Typekind::Recall(istr, m, None) => {
                tara.quir_types[self] = match istr {
                    s if s == "int".into() => quir::Typekind::Int,
                    s if s == "string".into() => quir::Typekind::String,
                    s if s == "bool".into() => quir::Typekind::Bool,
                    s if s == "tuple".into() => quir::Typekind::Tup,
                    _ => {
                        let Some(&id) = globals.get(&istr) else {
                            tara.report(
                                message!(
                                    error @loc
                                        => "Could not resolve name '{}'!", istr.0
                                ),
                                &[],
                            );
                            todo!("poisoning?");
                        };
                        quir::Typekind::Recall(istr, m, Some(id))
                    }
                }
            }
            quir::Typekind::Recall(_, _, Some(_))
            | quir::Typekind::Var(_)
            | quir::Typekind::String
            | quir::Typekind::Bool
            | quir::Typekind::Int
            | quir::Typekind::Tup => {}
        }
    }
}

pub(super) struct Ctx<'a> {
    owner: LocalvecId,
    locals: Svec<Istr, BindingId>,
    loops: Vec<ExprId>,
    tara: &'a mut Tara,
    globals: &'a Map<Istr, quir::Id>,
}
impl std::ops::Index<BindingId> for Ctx<'_> {
    type Output = Binding;
    fn index(&self, i: BindingId) -> &Self::Output {
        &self.tara.quir_items[self.owner][i]
    }
}
impl std::ops::IndexMut<BindingId> for Ctx<'_> {
    fn index_mut(&mut self, i: BindingId) -> &mut Self::Output {
        &mut self.tara.quir_items[self.owner][i]
    }
}
impl std::ops::Index<ExprId> for Ctx<'_> {
    type Output = Expr;
    fn index(&self, i: ExprId) -> &Self::Output {
        &self.tara.quir_items[self.owner][i]
    }
}
impl std::ops::IndexMut<ExprId> for Ctx<'_> {
    fn index_mut(&mut self, i: ExprId) -> &mut Self::Output {
        &mut self.tara.quir_items[self.owner][i]
    }
}
impl<'a> Ctx<'a> {
    fn new(owner: LocalvecId, globals: &'a Map<Istr, quir::Id>, tara: &'a mut Tara) -> Self {
        Self {
            locals: Svec::default(),
            loops: Vec::default(),
            globals,
            owner,
            tara,
        }
    }
    fn get_name(&self, n: Istr) -> Option<Either<BindingId, quir::Id>> {
        match self.locals.find(&n) {
            Some(&bid) => Some(Left(bid)),
            None => self.globals.get(&n).map(|&id| Right(id)),
        }
    }
}

impl BindingId {
    fn resolve(self, ctx: &mut Ctx) {
        let typ = ctx[self].typ;
        typ.resolve(ctx.globals, ctx.tara);
        match ctx[self].kind {
            binding::Kind::Empty(_) => {}
            binding::Kind::Name(_) => {}
            binding::Kind::Tuple(ref tuple) => {
                for &b in tuple.0.cheap().iter() {
                    b.resolve(ctx);
                }
            }
        }
    }
    fn register(self, ctx: &mut Ctx) {
        match ctx[self].kind {
            binding::Kind::Empty(_) => {}
            binding::Kind::Name(n) => {
                ctx.locals.insert(n.0, self);
            }
            binding::Kind::Tuple(ref bs) => {
                for &b in bs.0.cheap().iter() {
                    b.register(ctx);
                }
            }
        }
    }
}

impl ExprId {
    fn resolve(self, ctx: &mut Ctx) {
        let typ = ctx[self].typ;
        typ.resolve(ctx.globals, ctx.tara);
        ctx[self].kind.cheap().resolve(self, ctx);
    }
}

impl expr::If {
    pub(super) fn resolve(&self, _: ExprId, ctx: &mut Ctx) {
        self.cond.resolve(ctx);
        self.smash.resolve(ctx);
        self.pass.resolve(ctx);
    }
}

impl expr::Call {
    pub(super) fn resolve(&self, _: ExprId, ctx: &mut Ctx) {
        self.func.resolve(ctx);
        self.args.resolve(ctx);
    }
}

impl expr::Builtinkind {
    pub(super) fn resolve(&self, _: ExprId, _: &mut Ctx) {}
}

impl expr::Tuple {
    pub(super) fn resolve(&self, _: ExprId, ctx: &mut Ctx) {
        for &e in self.0.cheap().iter() {
            e.resolve(ctx);
        }
    }
}

impl expr::Loop {
    pub(super) fn resolve(&self, this: ExprId, ctx: &mut Ctx) {
        ctx.loops.push(this);
        self.0.resolve(ctx);
        ctx.loops.pop();
    }
}

impl expr::Bareblock {
    pub(super) fn resolve(&self, _: ExprId, ctx: &mut Ctx) {
        let len = ctx.locals.len();
        for &e in self.0.cheap().iter() {
            e.resolve(ctx);
        }
        ctx.locals.truncate(len);
    }
}

impl expr::Recall {
    pub(super) fn resolve(&self, this: ExprId, ctx: &mut Ctx) {
        if self.1.is_some() {
            return;
        }
        if self.0.len() == 1 {
            for &builtin in expr::Builtinkind::VALUES {
                if builtin.spelling() == self.0[0].name.0 {
                    ctx[this].kind = builtin.into();
                    return;
                }
            }
        }
        let Some(mut target) = ctx.get_name(self.0[0].name) else {
            ctx.tara.report(
                message!(
                    error @ self.0[0].loc
                        => "Could not resolve name '{}'", self.0[0].name.0
                ),
                &[],
            );
            todo!("poisoning?")
        };
        if self.0.len() > 1 && target.is_left() {
            ctx.tara.report(
                message!(
                    error @ self.0[1].loc
                        => "Cannot index locals!"
                ),
                &[message!(
                    note @ ctx[target.unwrap_left()].loc
                        => "Local declared here!"
                )],
            );
            todo!("poisoning?")
        }
        for &n in self.0[1..].iter() {
            let target_right = target.unwrap_right();
            match target_right.index(n.name, ctx.tara) {
                Ok(Some(i)) => target = Right(i),
                Ok(None) => {
                    ctx.tara.report(
                        message!(
                            error @ n.loc
                                => "Could not resolve name '{}'!", n.name.0
                        ),
                        &[],
                    );
                    todo!("poisoning?")
                }
                Err(_) => {
                    ctx.tara.report(
                        message!(
                            error @ n.loc
                                => "Could not index a function!"
                        ),
                        &[message!(
                            note @ ctx.tara.quir_items[target_right].loc()
                                => "Declared here!"
                        )],
                    );
                    todo!("poisoning?")
                }
            }
        }
        ctx[this].kind = expr::Recall(self.0.cheap(), Some(target)).into();
    }
}

impl expr::Number {
    pub(super) fn resolve(&self, _: ExprId, _: &mut Ctx) {}
}

impl expr::String {
    pub(super) fn resolve(&self, _: ExprId, _: &mut Ctx) {}
}

impl expr::Bool {
    pub(super) fn resolve(&self, _: ExprId, _: &mut Ctx) {}
}

impl expr::Arguments {
    pub(super) fn resolve(&self, _: ExprId, _: &mut Ctx) {}
}

impl expr::Poison {
    pub(super) fn resolve(&self, _: ExprId, _: &mut Ctx) {}
}

impl expr::Let {
    pub(super) fn resolve(&self, _: ExprId, ctx: &mut Ctx) {
        self.0.resolve(ctx);
        self.1.resolve(ctx);
        self.0.register(ctx);
    }
}

impl expr::Assign {
    pub(super) fn resolve(&self, this: ExprId, ctx: &mut Ctx) {
        if self.1.is_some() {
            return;
        }
        self.2.resolve(ctx);
        let bid = match ctx.get_name(self.0.name) {
            Some(Left(bid)) => bid,
            Some(Right(gid)) => {
                ctx.tara.report(
                    message!(
                        error @ ctx[this].loc
                            => "Cannot assign to a global!"
                    ),
                    &[message!(
                        note @ ctx.tara.quir_items[gid].loc()
                            => "Global declared here!"
                    )],
                );
                todo!("poisoning!!");
            }
            None => {
                ctx.tara.report(
                    message!(
                        error @ self.0.loc
                            => "Could not resolve name '{}'!", self.0.name.0
                    ),
                    &[],
                );
                todo!("poisoning!!");
            }
        };
        ctx[this].kind = expr::Assign(self.0, Some(bid), self.2).into();
    }
}

impl expr::Break {
    pub(super) fn resolve(&self, this: ExprId, ctx: &mut Ctx) {
        if self.target.is_some() {
            return;
        }
        self.val.resolve(ctx);
        let target = match ctx.loops.last() {
            Some(&id) => id,
            None => {
                ctx.tara.report(
                    message!(
                        error @ ctx[this].loc
                            => "Break is not part of any loop!"
                    ),
                    &[message!(
                        note => "'break' currently only escapes a loop!"
                    )],
                );
                todo!("poisoning!");
            }
        };
        ctx[this].kind = expr::Break {
            target: Some(target),
            val: self.val,
        }
        .into();
    }
}

impl expr::Return {
    pub(super) fn resolve(&self, _: ExprId, ctx: &mut Ctx) {
        self.0.resolve(ctx);
    }
}

impl expr::Const {
    pub(super) fn resolve(&self, _: ExprId, ctx: &mut Ctx) {
        self.0.resolve(ctx);
    }
}
