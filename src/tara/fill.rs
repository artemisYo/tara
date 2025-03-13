use std::rc::Rc;

use crate::{misc::Ivec, typer};

use super::{
    uir::{self, BindingId, ExprId, Function, TypeId},
    Tara,
};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct In {
    pub i: uir::Id,
}

impl Tara {
    pub fn fill(&mut self, i: In) {
        if self.fills.contains(&i) {
            return;
        }
        self.fills.insert(i);
        fill(self, i);
    }
}

struct Context<'a> {
    f: &'a mut Function,
    typevec: &'a mut Ivec<TypeId, uir::Typekind>,
    s: Rc<std::collections::BTreeMap<usize, TypeId>>,
}

fn fill(ctx: &mut Tara, i: In) {
    let substs = ctx.typeck(typer::In { i: i.i });
    let f = &mut ctx.uir_items[i.i];
    let b = f.body;
    Context {
        s: substs.substitutions,
        typevec: &mut ctx.uir_types,
        f,
    }
    .expressions(b);
}

impl Context<'_> {
    fn expressions(&mut self, b: ExprId) {
        self.f.locals[b]
            .typ
            .kind
            .replace(self.typevec, self.s.as_ref());
        match self.f.locals[b].kind {
            uir::Exprkind::If { cond, smash, pass } => {
                self.expressions(cond);
                self.expressions(smash);
                self.expressions(pass);
            }
            uir::Exprkind::Call { func, args } => {
                self.expressions(func);
                self.expressions(args);
            }
            uir::Exprkind::Builtin { args, .. } => {
                self.expressions(args);
            },
            uir::Exprkind::Tuple(ref expr_ids) | uir::Exprkind::Bareblock(ref expr_ids) => {
                for &e in expr_ids.clone().iter() {
                    self.expressions(e);
                }
            }
            uir::Exprkind::Arguments
            | uir::Exprkind::Poison
            | uir::Exprkind::Recall(_)
            | uir::Exprkind::Number(_)
            | uir::Exprkind::String(_)
            | uir::Exprkind::Bool(_) => {}
            uir::Exprkind::Assign(binding_id, expr_id)
            | uir::Exprkind::Let(binding_id, expr_id) => {
                self.bindings(binding_id);
                self.expressions(expr_id);
            }
            uir::Exprkind::Loop(val)
            | uir::Exprkind::Break { val, .. }
            | uir::Exprkind::Return(val)
            | uir::Exprkind::Const(val) => {
                self.expressions(val);
            }
        }
    }

    fn bindings(&mut self, b: BindingId) {
        self.f.locals[b]
            .typ
            .kind
            .replace(self.typevec, self.s.as_ref());
        match &self.f.locals[b].kind {
            uir::Bindkind::Empty | uir::Bindkind::Name(_, _) => {}
            uir::Bindkind::Tuple(binding_ids) => {
                for &b in binding_ids.clone().iter() {
                    self.bindings(b);
                }
            }
        }
    }
}
