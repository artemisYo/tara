pub mod exec;
pub mod syntax;

pub type Root = File;

#[derive(Debug)]
enum Type {
	Int,
	Void,
}

#[derive(Debug)]
pub struct File {
    body: Vec<Statement>,
    tail: Option<Expr>,
    typing: Option<Type>
}
#[derive(Debug)]
pub enum Statement {
    Let(LetStmt),
    Expr(Expr),
}
impl Statement {
	fn typing(&self) -> &Option<Type> {
		match self {
			Self::Let(l) => &l.typing,
			Self::Expr(e) => e.typing(),
		}
	}
}
#[derive(Debug)]
pub struct LetStmt {
    name: String,
    init: Expr,
    typing: Option<Type>
}
#[derive(Debug)]
pub enum Expr {
    Binary(BinExpr),
    Single(SinExpr),
    While(WhileExpr),
    If(IfExpr),
}
impl Expr {
	fn typing(&self) -> &Option<Type> {
		match self {
			Self::Binary(b) => &b.typing,
			Self::Single(s) => s.typing(),
			Self::While(w) => &w.typing,
			Self::If(i) => &i.typing,
		}
	}
}
#[derive(Debug)]
pub struct Block(Box<File>);
impl Block {
	fn typing(&self) -> &Option<Type> {
		&self.0.typing
	}
}
#[derive(Debug)]
pub struct IfExpr {
    cond: Box<Expr>,
    smash: Block,
    pass: Option<Block>,
    typing: Option<Type>,
}
#[derive(Debug)]
pub struct WhileExpr {
    cond: Box<Expr>,
    body: Block,
    then: Option<Block>,
    typing: Option<Type>,
}
#[derive(Debug)]
pub struct BinExpr {
    op: BinOp,
    args: [Box<Expr>; 2],
    typing: Option<Type>,
}
#[derive(Debug)]
pub enum BinOp {
    Plus,
    Minus,
    Star,
    Slash,
}
#[derive(Debug)]
pub enum SinExpr {
    Number(isize),
    Name(String),
}
impl SinExpr {
	fn typing(&self) -> &Option<Type> {
		match self {
			Self::Number(_) => &Some(Type::Int),
			Self::Name(_) => &Some(Type::Int),
		}
	}
}
