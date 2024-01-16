use std::collections::BTreeMap;

use super::*;

pub fn run(ast: Root) -> isize {
    ast.run(&mut Ctx::new())
}

struct Ctx<'a> {
	decls: BTreeMap<&'a str, isize>,
}
impl<'a> Ctx<'a> {
	fn new() -> Self {
		Self { decls: BTreeMap::new() }
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
			Self::Expr(_) => {}, // no effects yet
			Self::Let(l) => l.run(ctx)
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
        while self.cond.run(ctx) != 0 {
            self.body.run(ctx);
        }
        self.then.as_ref().map(|p| p.run(ctx)).unwrap_or(0)
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
			Self::Name(n) => *ctx.decls.get(n.as_str()).expect("use of undeclared name!")
        }
    }
}
