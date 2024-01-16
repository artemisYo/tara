pub mod syntax;
pub mod exec;

pub type Root = File;

#[derive(Debug)]
pub struct File {
	body: Vec<Statement>,
	tail: Option<Expr>,
}
#[derive(Debug)]
pub enum Statement {
    Let(LetStmt),
	Expr(Expr),
}
#[derive(Debug)]
pub struct LetStmt {
	name: String,
	init: Expr,
}
#[derive(Debug)]
pub enum Expr {
    Binary(BinExpr),
    Single(SinExpr),
    While(WhileExpr),
    If(IfExpr),
}
#[derive(Debug)]
pub struct IfExpr {
    cond: Box<Expr>,
    smash: Box<File>,
    pass: Option<Box<File>>,
}
#[derive(Debug)]
pub struct WhileExpr {
    cond: Box<Expr>,
    body: Box<File>,
    then: Option<Box<File>>,
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
    Number(isize),
    Name(String),
}
