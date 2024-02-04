pub mod exec;
<<<<<<< HEAD
pub mod gen;
=======
>>>>>>> parent of a2da5e6 (Stub of asm generation)
pub mod syntax;
pub mod typing;
use typing::Type;

pub type Root = File;

#[derive(Debug)]
pub struct File {
    body: Vec<Statement>,
    tail: Box<Expr>,
    typing: Option<Type>
}
impl File {
	fn empty() -> Self {
		Self {
    		body: Vec::new(),
        	tail: Box::new(Expr::Single(SinExpr::Empty)),
        	typing: Some(Type::Void)
		}
	}
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
    typing: Option<Type>
}
#[derive(Debug)]
pub enum Expr {
    Binary(BinExpr),
    Single(SinExpr),
    While(WhileExpr),
    If(IfExpr),
}
#[derive(Debug)]
pub struct Block(File);

#[derive(Debug)]
pub struct IfExpr {
    cond: Box<Expr>,
    smash: Block,
    pass: Block,
    typing: Option<Type>,
}

#[derive(Debug)]
pub struct WhileExpr {
    cond: Box<Expr>,
    smashing: Block,
    pass: Block,
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
    Empty,
}

