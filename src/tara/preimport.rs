use std::rc::Rc;

use crate::{
    gen_query, message, misc::{CheapClone, Istr}, prescan::Opdef, tara::prescan, ModuleId, Provenance, Tara
};

#[derive(Debug)]
pub struct Import {
    pub target: ModuleId,
    pub head: Option<Istr>,
    pub loc: Provenance,
}

#[derive(Clone)]
pub struct Out {
    pub ops: Rc<[Opdef]>,
    pub imports: Rc<[Import]>,
}
impl CheapClone for Out {}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct In {
    pub m: ModuleId,
}

gen_query!(preimport);
fn preimport(ctx: &mut Tara, i: In) -> Out {
    let mut imports = Vec::new();
    let mut prescan = ctx.prescan(prescan::In { m: i.m });
    for (loc, path) in prescan.imports.as_ref() {
        if path.len() < 2 {
            ctx.report(
                message!(
                    error @ loc.meet(&path.first().map_or(*loc, |t| t.0))
                        => "This import does nothing!"
                ),
                &[message!(note => "Remove the import")],
            );
            continue;
        }
        let mut target = ctx.get_sibling(i.m, &path[0].1);
        for p in &path[1..path.len() - 1] {
            target = ctx.get_child(target, &p.1);
        }
        let head = path[path.len() - 1].1;
        imports.push(Import {
            target,
            head: Some(head).filter(|&s| s != "...".into()),
            loc: *loc,
        })
    }
    for i in &imports {
        let i = ctx.preimport(In { m: i.target });
        prescan.ops.extend(i.ops.iter());
    }
    Out {
        ops: prescan.ops.into(),
        imports: imports.into(),
    }
}
