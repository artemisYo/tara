use crate::scoped_map::*;

use super::*;

pub fn run(ast: Root) -> isize {
    ast.run(&mut Ctx::new())
}

struct Ctx<'a> {
    decls: ScopeMap<&'a str, isize>,
}
impl<'a> Ctx<'a> {
    fn new() -> Self {
        Self {
            decls: ScopeMap::new(),
        }
    }
}

impl File {
    fn run<'a>(&'a self, ctx: &mut Ctx<'a>) -> isize {
        self.body.iter().for_each(|s| s.run(ctx));
        self.tail.as_ref().map(|e| e.run(ctx)).unwrap_or(0)
    }
}

impl Statement {
    fn run<'a>(&'a self, ctx: &mut Ctx<'a>) {
        match self {
            Self::Expr(_) => {} // no effects yet
            Self::Let(l) => l.run(ctx),
        }
    }
}

impl LetStmt {
    fn run<'a>(&'a self, ctx: &mut Ctx<'a>) {
        let val = self.init.run(ctx);
        ctx.decls.insert(&self.name, val);
    }
}

impl Expr {
    fn run<'a>(&'a self, ctx: &mut Ctx<'a>) -> isize {
        match self {
            Expr::Binary(b) => b.run(ctx),
            Expr::Single(s) => s.run(ctx),
            Expr::If(i) => i.run(ctx),
            Expr::While(w) => w.run(ctx),
        }
    }
}

impl Block {
    fn run<'a>(&'a self, ctx: &mut Ctx<'a>) -> isize {
        let scope = ctx.decls.scope();
        let out = self.0.run(ctx);
        ctx.decls.restore(scope);
        out
    }
    fn run_no_scope<'a>(&'a self, ctx: &mut Ctx<'a>) -> isize {
        self.0.run(ctx)
    }
}

impl IfExpr {
    fn run<'a>(&'a self, ctx: &mut Ctx<'a>) -> isize {
        let cond = self.cond.run(ctx);
        if cond != 0 {
            self.smash.run(ctx)
        } else {
            self.pass.as_ref().map(|p| p.run(ctx)).unwrap_or(0)
        }
    }
}

impl WhileExpr {
    fn run<'a>(&'a self, ctx: &mut Ctx<'a>) -> isize {
        let mut ran = false;
        let mut scope = ctx.decls.scope();
        while self.cond.run(ctx) != 0 {
            ctx.decls.restore(scope);
            scope = ctx.decls.scope();
            ran = true;
            self.body.run_no_scope(ctx);
        }
        if ran {
            let out = self.then.as_ref().map(|p| p.run(ctx)).unwrap_or(0);
            ctx.decls.restore(scope);
            out
        } else {
            0
        }
    }
}

impl BinExpr {
    fn run<'a>(&'a self, ctx: &mut Ctx<'a>) -> isize {
        let [lhs, rhs] = &self.args;
        match self.op {
            BinOp::Plus => lhs.run(ctx) + rhs.run(ctx),
            BinOp::Minus => lhs.run(ctx) - rhs.run(ctx),
            BinOp::Star => lhs.run(ctx) * rhs.run(ctx),
            BinOp::Slash => lhs.run(ctx) / rhs.run(ctx),
        }
    }
}
impl SinExpr {
    fn run(&self, ctx: &mut Ctx) -> isize {
        match self {
            Self::Number(n) => *n,
            Self::Name(n) => *ctx.decls.get(&n.as_str()).expect("use of undeclared name!"),
        }
    }
}
