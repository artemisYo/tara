use std::rc::Rc;

use crate::{gen_query, misc::CheapClone, typer};

use super::{
    quir::{self, binding, expr, Binding, BindingId, Expr, ExprId, Type, TypeId},
    Tara,
};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub struct In {
    pub i: quir::Id,
}

#[derive(Clone, Copy)]
pub struct Out {
    pub typ: Option<Type>,
}
impl CheapClone for Out {}

type Substs = Rc<std::collections::BTreeMap<usize, TypeId>>;

gen_query!(fill);
fn fill(tara: &mut Tara, In { i }: In) -> Out {
    let substs = tara.typecheck(typer::In { i }).substitutions;
    i.fill(&substs, tara);
    let typ = match tara.quir_items[i] {
        quir::Item::Typecase(ref c) => Some(c.typ),
        quir::Item::Function(ref f) => Some(f.typ),
        quir::Item::Typedecl(_) => None,
        quir::Item::Namespace(_) => None,
    };
    Out { typ }
}

impl quir::Id {
    fn fill(self, substs: &Substs, tara: &mut Tara) {
        match tara.quir_items[self] {
            quir::Item::Typecase(_) | quir::Item::Typedecl(_) => {}
            quir::Item::Function(ref f) => f.body.fill(&mut Ctx {
                owner: self.into_localvec(),
                substs: substs.cheap(),
                tara,
            }),
            quir::Item::Namespace(ref n) => {
                for &i in n.items.cheap().values() {
                    i.fill(substs, tara);
                }
            }
        }
    }
}

pub struct Ctx<'a> {
    owner: quir::LocalvecId,
    substs: Substs,
    tara: &'a mut Tara,
}
impl std::ops::Index<ExprId> for Ctx<'_> {
    type Output = Expr;
    fn index(&self, index: ExprId) -> &Self::Output {
        &self.tara.quir_items[self.owner][index]
    }
}
impl std::ops::IndexMut<ExprId> for Ctx<'_> {
    fn index_mut(&mut self, index: ExprId) -> &mut Self::Output {
        &mut self.tara.quir_items[self.owner][index]
    }
}
impl std::ops::Index<BindingId> for Ctx<'_> {
    type Output = Binding;
    fn index(&self, index: BindingId) -> &Self::Output {
        &self.tara.quir_items[self.owner][index]
    }
}
impl std::ops::IndexMut<BindingId> for Ctx<'_> {
    fn index_mut(&mut self, index: BindingId) -> &mut Self::Output {
        &mut self.tara.quir_items[self.owner][index]
    }
}

impl ExprId {
    fn fill(self, ctx: &mut Ctx) {
        let kind = ctx[self].typ.kind;
        ctx.tara.replace(kind, ctx.substs.as_ref());
        ctx[self].kind.cheap().fill(ctx);
    }
}

impl expr::If {
    pub fn fill(&self, ctx: &mut Ctx) {
        self.cond.fill(ctx);
        self.smash.fill(ctx);
        self.pass.fill(ctx);
    }
}

impl expr::Call {
    pub fn fill(&self, ctx: &mut Ctx) {
        self.func.fill(ctx);
        self.args.fill(ctx);
    }
}

impl expr::Bareblock {
    pub fn fill(&self, ctx: &mut Ctx) {
        for &e in self.0.cheap().iter() {
            e.fill(ctx);
        }
    }
}

impl expr::Tuple {
    pub fn fill(&self, ctx: &mut Ctx) {
        for &e in self.0.cheap().iter() {
            e.fill(ctx);
        }
    }
}

impl expr::Let {
    pub fn fill(&self, ctx: &mut Ctx) {
        self.0.fill(ctx);
        self.1.fill(ctx);
    }
}

impl expr::Assign {
    pub fn fill(&self, ctx: &mut Ctx) {
        self.1.expect("resolve shoulda").fill(ctx);
        self.2.fill(ctx);
    }
}

impl expr::Loop {
    pub fn fill(&self, ctx: &mut Ctx) {
        self.0.fill(ctx)
    }
}

impl expr::Break {
    pub fn fill(&self, ctx: &mut Ctx) {
        self.val.fill(ctx)
    }
}

impl expr::Return {
    pub fn fill(&self, ctx: &mut Ctx) {
        self.0.fill(ctx)
    }
}

impl expr::Const {
    pub fn fill(&self, ctx: &mut Ctx) {
        self.0.fill(ctx)
    }
}

impl expr::Poison {
    pub fn fill(&self, _: &mut Ctx) {
        todo!("poisoning")
    }
}

impl expr::Builtinkind {
    pub fn fill(&self, _: &mut Ctx) {}
}
impl expr::Recall {
    pub fn fill(&self, _: &mut Ctx) {}
}
impl expr::Number {
    pub fn fill(&self, _: &mut Ctx) {}
}
impl expr::String {
    pub fn fill(&self, _: &mut Ctx) {}
}
impl expr::Bool {
    pub fn fill(&self, _: &mut Ctx) {}
}
impl expr::Arguments {
    pub fn fill(&self, _: &mut Ctx) {}
}

impl BindingId {
    fn fill(self, ctx: &mut Ctx) {
        let kind = ctx[self].typ.kind;
        ctx.tara.replace(kind, ctx.substs.as_ref());
        ctx[self].kind.cheap().fill(ctx);
    }
}

impl binding::Empty {
    pub fn fill(&self, _: &mut Ctx) {}
}
impl binding::Name {
    pub fn fill(&self, _: &mut Ctx) {}
}
impl binding::Tuple {
    pub fn fill(&self, ctx: &mut Ctx) {
        for &b in self.0.cheap().iter() {
            b.fill(ctx);
        }
    }
}
