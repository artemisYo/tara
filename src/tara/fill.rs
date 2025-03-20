use std::rc::Rc;

use crate::typer;

use super::{
    uir::{self, BindingId, ExprId, TypeId},
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

type Substs = Rc<std::collections::BTreeMap<usize, TypeId>>;

fn fill(tara: &mut Tara, i: In) {
    let substs = tara.typeck(typer::In { i: i.i }).substitutions;
    match tara.uir_items[i.i] {
        uir::Item::Function(ref f) => f.body.fill(i.i, &substs, tara),
        uir::Item::Typedecl(_) => {}
    }
}

impl ExprId {
    fn fill(self, owner: uir::Id, substs: &Substs, tara: &mut Tara) {
        tara[(owner, self)]
            .typ
            .kind
            .replace(&mut tara.uir_types, substs);
        match tara[(owner, self)].kind {
            uir::Exprkind::If { cond, smash, pass } => {
                cond.fill(owner, substs, tara);
                smash.fill(owner, substs, tara);
                pass.fill(owner, substs, tara);
            }
            uir::Exprkind::Call { func, args } => {
                func.fill(owner, substs, tara);
                args.fill(owner, substs, tara);
            }
            uir::Exprkind::Bareblock(ref ids) | uir::Exprkind::Tuple(ref ids) => {
                for &e in ids.clone().iter() {
                    e.fill(owner, substs, tara);
                }
            }
            uir::Exprkind::Builtin(_)
            | uir::Exprkind::Recall(_)
            | uir::Exprkind::Number(_)
            | uir::Exprkind::String(_)
            | uir::Exprkind::Bool(_)
            | uir::Exprkind::Arguments => {}
            uir::Exprkind::Let(bid, eid) | uir::Exprkind::Assign(bid, eid) => {
                bid.fill(owner, substs, tara);
                eid.fill(owner, substs, tara);
            }
            uir::Exprkind::Loop(val)
            | uir::Exprkind::Break { val, .. }
            | uir::Exprkind::Return(val)
            | uir::Exprkind::Const(val) => val.fill(owner, substs, tara),
            uir::Exprkind::Poison => std::process::exit(1),
        }
    }
}

impl BindingId {
    fn fill(self, owner: uir::Id, substs: &Substs, tara: &mut Tara) {
        tara[(owner, self)]
            .typ
            .kind
            .replace(&mut tara.uir_types, substs);
        match tara[(owner, self)].kind {
            uir::Bindkind::Empty | uir::Bindkind::Name(_, _) => {}
            uir::Bindkind::Tuple(ref bids) => {
                for &b in bids.clone().iter() {
                    b.fill(owner, substs, tara);
                }
            }
        }
    }
}
