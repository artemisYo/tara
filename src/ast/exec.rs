use super::*;

pub fn run(ast: Root) -> isize {
    ast.run()
}

impl Expr {
    fn run(self) -> isize {
        match self {
            Expr::Binary(b) => b.run(),
            Expr::Single(s) => s.run(),
        }
    }
}
impl BinExpr {
    fn run(self) -> isize {
        let [lhs, rhs] = self.args;
        match self.op {
            BinOp::Plus => lhs.run() + rhs.run(),
            BinOp::Minus => lhs.run() - rhs.run(),
            BinOp::Star => lhs.run() * rhs.run(),
            BinOp::Slash => lhs.run() / rhs.run(),
        }
    }
}
impl SinExpr {
    fn run(self) -> isize {
        match self {
            Self::Number(n) => n
        }
    }
}
