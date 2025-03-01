use std::rc::Rc;

use crate::{ansi::Style, misc::Istr, prescan::Opdef, tara::prescan, ModuleId, Tara};

#[derive(Debug)]
pub struct Import {
    target: ModuleId,
    head: Option<Istr>,
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
            Some(o) => o.clone(),
            None => {
                let data = preimport(self, i);
                self.preimports.insert(i, data);
                self.preimports.get(&i).unwrap().clone()
            }
        }
    }
}

fn preimport(ctx: &mut Tara, i: In) -> Out {
    let mut imports = Vec::new();
    let mut prescan = ctx.prescan(prescan::In { m: i.m });
    for (loc, path) in prescan.imports.as_ref() {
        if path.len() < 2 {
            loc.meet(&path.first().map_or(*loc, |t| t.0)).report(
                &ctx,
                Style::yellow() | Style::underline(),
                Style::yellow().apply("Note"),
                "This import does nothing!",
                ["Remove the import"].into_iter(),
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
        prescan.ops.extend(i.ops.into_iter());
    }
    Out {
        ops: prescan.ops.into(),
        imports: imports.into(),
    }
}
