use std::collections::BTreeMap;

use super::*;

pub fn run(ast: Root) -> isize {
    ast.run(&mut Ctx::new())
}

struct Ctx {
	decls: BTreeMap<String, isize>,
}
impl Ctx {
	fn new() -> Self {
		Self { decls: BTreeMap::new() }
	}
}

impl File {
	fn run(self, ctx: &mut Ctx) -> isize {
		self.body.into_iter().for_each(|s| s.run(ctx));
		self.tail.map(|e| e.run(ctx)).unwrap_or(0)
	}
}

impl Statement {
	fn run(self, ctx: &mut Ctx) {
		match self {
			Self::Expr(_) => {}, // no effects yet
			Self::Let(l) => l.run(ctx)
		}
	}
}

impl LetStmt {
	fn run(self, ctx: &mut Ctx) {
    	let val = self.init.run(ctx);
		ctx.decls.insert(self.name, val);
	}
}

impl Expr {
    fn run(self, ctx: &mut Ctx) -> isize {
        match self {
            Expr::Binary(b) => b.run(ctx),
            Expr::Single(s) => s.run(ctx),
        }
    }
}
impl BinExpr {
    fn run(self, ctx: &mut Ctx) -> isize {
        let [lhs, rhs] = self.args;
        match self.op {
            BinOp::Plus => lhs.run(ctx) + rhs.run(ctx),
            BinOp::Minus => lhs.run(ctx) - rhs.run(ctx),
            BinOp::Star => lhs.run(ctx) * rhs.run(ctx),
            BinOp::Slash => lhs.run(ctx) / rhs.run(ctx),
        }
    }
}
impl SinExpr {
    fn run(self, ctx: &mut Ctx) -> isize {
        match self {
            Self::Number(n) => n,
			Self::Name(n) => *ctx.decls.get(&n).expect("use of undeclared name!")
        }
    }
}
