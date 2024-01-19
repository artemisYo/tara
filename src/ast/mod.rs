pub mod exec;
pub mod syntax;

pub type Root = File;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Type {
	Int,
	Void,
}

#[derive(Debug)]
pub struct File {
    body: Vec<Statement>,
    tail: Box<Expr>,
    typing: Option<Type>
}
impl File {
	fn typing(&mut self) -> &Type {
		if let Some(t) = &self.typing {
			return t;
		}
		let t = self.tail.typing();
		self.typing = Some(t.clone());
		&t
	}
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
impl Statement {
	fn typing(&self) -> &Type {
		match self {
			Self::Let(l) => l.typing(),
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
impl LetStmt {
	fn typing(&self) -> &Type {
		if let Some(t) = self.typing.as_ref() {
			return t;
		}
		let t = self.init.typing();
		self.typing = Some(t.clone());
		t
	}
}
#[derive(Debug)]
pub enum Expr {
    Binary(BinExpr),
    Single(SinExpr),
    While(WhileExpr),
    If(IfExpr),
}
impl Expr {
	fn typing(&self) -> &Type {
		match self {
			Self::Binary(b) => b.typing(),
			Self::Single(s) => s.typing(),
			Self::While(w) => w.typing(),
			Self::If(i) => i.typing(),
		}
	}
}
#[derive(Debug)]
pub struct Block(File);
impl Block {
	fn typing(&self) -> &Type {
		self.0.typing()
	}
}
#[derive(Debug)]
pub struct IfExpr {
    cond: Box<Expr>,
    smash: Block,
    pass: Block,
    typing: Option<Type>,
}
impl IfExpr {
	fn typing(&self) -> &Type {
    	let smash = self.smash.typing();
		let pass = self.pass.typing();
		let t = if smash != pass {
			Type::Void
		} else {
			smash.clone()
		};
		self.typing = Some(t);
		self.typing.as_ref().unwrap()
	}
}
#[derive(Debug)]
pub struct WhileExpr {
    cond: Box<Expr>,
    body: Block,
    then: Block,
    typing: Option<Type>,
}
impl WhileExpr {
	fn typing(&self) -> &Type {
		if let Some(t) = self.typing.as_ref() {
			return t;
		}
		let then = self.then.typing();
		self.typing = Some(then.clone());
		then
	}
}
#[derive(Debug)]
pub struct BinExpr {
    op: BinOp,
    args: [Box<Expr>; 2],
    typing: Option<Type>,
}
impl BinExpr {
	fn typing(&self) -> &Type {
		if let Some(t) = self.typing.as_ref() {
			return t;
		}
		let lhs = self.args[0].typing();
		let rhs = self.args[1].typing();
		let t = self.op.typing(lhs, rhs);
		self.typing = Some(t);
		self.typing()
	}
}
#[derive(Debug)]
pub enum BinOp {
    Plus,
    Minus,
    Star,
    Slash,
}
impl BinOp {
	fn typing(&self, lhs: &Type, rhs: &Type) -> Type {
    	assert!(lhs == &Type::Int);
    	assert!(rhs == &Type::Int);
    	Type::Int
	}
}
#[derive(Debug)]
pub enum SinExpr {
    Number(isize),
    Name(String),
    Empty,
}
impl SinExpr {
	fn typing(&self) -> &Type {
		match self {
			Self::Number(_) => &Type::Int,
			Self::Name(_) => &Type::Int,
			Self::Empty => &Type::Void,
		}
	}
}
