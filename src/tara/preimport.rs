use std::rc::Rc;

use crate::{message, misc::Istr, prescan::Opdef, tara::prescan, ModuleId, Tara};

#[derive(Debug)]
pub struct Import {
    pub target: ModuleId,
    pub head: Option<Istr>,
}

#[derive(Clone)]
pub struct Out {
    pub ops: Rc<[Opdef]>,
    pub imports: Rc<[Import]>,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct In {
    pub m: ModuleId,
}

impl Tara {
    pub fn preimport(&mut self, i: In) -> Out {
        match self.preimports.get(&i) {
            Some(Some(o)) => o.clone(),
            Some(None) => panic!("preimport entered a cycle!"),
            None => {
                // 'reserve' the spot, so that if the key is ever seen again,
                // we know it's a cycle
                self.preimports.insert(i, None);
                let data = preimport(self, i);
                self.preimports.insert(i, Some(data));
                self.preimports.get(&i).cloned().unwrap().unwrap()
            }
        }
    }
}

fn preimport(ctx: &mut Tara, i: In) -> Out {
    let mut imports = Vec::new();
    let mut prescan = ctx.prescan(prescan::In { m: i.m });
    for (loc, path) in prescan.imports.as_ref() {
        if path.len() < 2 {
            // ctx.report(
            //     Message::note(
            //         "This import does nothing!",
            //         Some(loc.meet(&path.first().map_or(*loc, |t| t.0))),
            //     ),
            //     &[Message::note("Remove the import", None)],
            // );
            ctx.report(
                message!(error @ loc.meet(&path.first().map_or(*loc, |t| t.0)) => "This import does nothing!"),
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
