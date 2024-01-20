use super::*;
use crate::scoped_map::*;
use std::io::Write;

pub fn to_asm(root: Root) -> String {
    let mut ctx = Ctx::new();
    root.to_asm(&mut ctx);
    ctx.buffer
}

struct Ctx {
    buffer: String,
    decls: ScopeMap<String, ()>,
}
impl Ctx {
    fn new() -> Self {
        Self {
            buffer: String::new(),
            decls: ScopeMap::new(),
        }
    }
}

impl File {
    fn to_asm(self, ctx: &mut Ctx) {
        self.body.into_iter().for_each(|s| s.to_asm(ctx));
        self.tail.to_asm(ctx);
    }
}

impl Statement {
    fn to_asm(self, ctx: &mut Ctx) {
        match self {
            Self::Let(l) => l.to_asm(ctx),
            Self::Expr(e) => e.to_asm(ctx),
        }
    }
}

impl LetStmt {
    fn to_asm(self, ctx: &mut Ctx) {
        ctx.decls.insert(self.name, ());
        self.init.to_asm(ctx);
        todo!("figure out the exact conventions for generating asm")
    }
}

impl Expr {
    fn to_asm(self, ctx: &mut Ctx) {
        todo!()
    }
}
