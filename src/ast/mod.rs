pub mod syntax;
pub mod exec;

pub type Root = Expr;

#[derive(Debug)]
pub enum Expr {
    Binary(BinExpr),
    Single(SinExpr),
}
#[derive(Debug)]
pub struct BinExpr {
    op: BinOp,
    args: [Box<Expr>;2]
}
#[derive(Debug)]
pub enum BinOp {
    Plus, Minus,
    Star, Slash
}
#[derive(Debug)]
pub enum SinExpr {
    Number(isize)
}
