use crate::tokenizer::{TokenKind, TokenView, TokenNode};
mod prism;
pub use prism::*;

pub fn parse(tt: TokenView) -> Option<File> {
	let mut parser = Parser {
		buffer: vec![],
	};
	parser.file(tt)?;
	Some(File(parser.buffer))
}

type PRes<'a> = Option<TokenView<'a>>;
struct Parser {
	buffer: Vec<TreeNode>,
}
impl Parser {
	fn with_scope<T: ViewBase>(&mut self, f: FnOnce(&mut Self, Validate<T>) -> T) -> T {
		todo!();
	}
	// File = (&:func Func)* ε
	fn file(&mut self, mut tt: TokenView) -> PRes {
		while !tt.is_empty() {
			match tt.peek().unwrap().kind {
				TokenKind::Func => {
					let rest = self.func(tt)?;
					tt = rest;
				},
				_ => todo!("error handling"),
			}
		}
		tt.is_empty().opt()?;
		Some(tt)
	}
	// Func = :func Name :paren{Params} (:arrRight Name)? :brace{Block}
	fn func(&mut self, mut tt: TokenView) -> PRes {
		tt.expect(TokenKind::Func)?;
		let func = self.buffer.len();
		self.buffer.push(TreeNode {
			size: 0,
			kind: TreeKind::Func,
		});
		let mut tt = self.name(tt)?;
		self.params(tt.children_if(TokenKind::Paren(0))?)?;
		tt.next();
		if tt.peek()?.kind == TokenKind::ArrRight {
			tt.next();
			tt = self.name(tt)?;
		} else {
			self.buffer.push(TreeNode {
				size: 1,
				kind: TreeKind::Void,
			});
		}
		self.block(tt.children_if(TokenKind::Brace(0))?)?;
		tt.next();
		self.buffer[func].size = self.buffer.len() - func;
		Some(tt)
	}
	// Params = (Name :colon Name :comma)* (Name :colon Name)? ε
	fn params(mut tt: TokenView) -> PRes {
		let vec = self.buffer.len();
		self.buffer.push(TreeNode {
			size: 0,
			kind: TreeKind::Vec,
		});
		while !tt.is_empty() {
			self.buffer.push(TreeNode {
				size: 2,
				kind: TreeKind::Pair,
			});
			tt = self.name(tt)?;
			tt.expect(TokenKind::Colon)?;
			tt = self.name(tt)?;
			if tt.peek()?.kind != TokenKind::Comma {
				break;
			}
		}
		tt.is_empty().opt()?;
		self.buffer[vec].size = self.buffer.len() - vec;
		Some(tt)
	}
	// Block = (Statement :semi)* Expr? ε
	fn block(&mut self, mut tt: TokenView) -> PRes {
		let vec = self.buffer.len();
		self.buffer.push(TreeNode {
			size: 0,
			kind: TreeKind::Vec,
		});
		while !tt.is_empty() {
			let size = self.buffer.len() - vec;
			tt = self.statement(tt)?;
			if tt.peek()?.kind == TokenKind::Semi {
				tt.next();
			} else {
				self.buffer[vec].size = size;
				break;
			}
		}
		// if Expr? didn't happen
		if self.buffer[vec].size == 0 {
			self.buffer[vec].size = self.buffer.len() - vec;
			self.buffer.push(TreeNode {
				size: 1,
				kind: TreeKind::Void,
			});
		}
		tt.is_empty().opt()?;
		Some(tt)
	}
	fn name(&mut self, mut tt: TokenView) -> PRes {
		let name = tt.expect(TokenKind::Name(0))?.string_idx()?;
		self.buffer.push(TreeNode {
			size: 1,
			kind: TreeKind::Name(name),
		});
		Some(tt)
	}
}

// Statement = (:let :name :eq Expr) / Expr
#[derive(Debug)]
pub enum Statement {
	Let(usize, Expr),
	Neither(Expr),
}
fn parse_statement(mut tt: TokenView) -> PRes<Statement> {
	match tt.peek()?.kind {
		TokenKind::Let => {
			tt.next();
			let name = tt.expect(TokenKind::Name(0))?.string_idx()?;
			tt.expect(TokenKind::Eq)?;
			let (tt, expr) = parse_expr(tt)?;
			Some((tt, Statement::Let(name, expr)))
		}
		_ => {
			let (tt, expr) = parse_expr(tt)?;
			Some((tt, Statement::Neither(expr)))
		}
	}
}

// Expr = BlockExpr / InlineExpr
#[derive(Debug)]
pub enum Expr {
	Block(BlockExpr),
	Inline(InlineExpr),
}
fn parse_expr(tt: TokenView) -> PRes<Expr> {
	parse_block_expr(tt).map(|(r, e)| (r, Expr::Block(e)))
		.or_else(|| {
			parse_inline_expr(tt).map(|(r, e)| (r, Expr::Inline(e)))
		})
}

// BlockExpr = :if Expr :brace{Block} (:else :brace{Block})?
#[derive(Debug)]
pub enum BlockExpr {
	IfExpr {
		cond: Box<Expr>,
		then: Box<Block>,
		other: Option<Box<Block>>
	},
}
fn parse_block_expr(mut tt: TokenView) -> PRes<BlockExpr> {
	tt.expect(TokenKind::If)?;
	let (mut tt, cond) = parse_expr(tt)?;
	let cond = Box::new(cond);
	let (_, then) = parse_block(tt.children_if(TokenKind::Brace(0))?)?;
	tt.next();
	let then = Box::new(then);
	let other = if tt.peek_if(TokenKind::Else).is_some() {
		tt.next();
		let (_, b) = parse_block(tt.children_if(TokenKind::Brace(0))?)?;
		tt.next();
		Some(Box::new(b))
	} else {
		None
	};
	Some((tt, BlockExpr::IfExpr { cond, then, other }))
}

// InlineExpr = BoolExpr
#[derive(Debug)]
pub enum InlineExpr {
	Bool(BoolExpr),
	Arith(Arithmetic),
	Func(FuncCall),
	Lit(LitExpr),
}
fn parse_inline_expr(tt: TokenView) -> PRes<InlineExpr> {
	parse_bool_expr(tt)
}

// BoolOp = :less / :more / :eqEq / :lessEq / :moreEq
// BoolExpr = Arithmetic (BoolOp Arithmetic)*
#[derive(Debug)]
pub struct BoolExpr {
	op: BoolOp,
	args: Box<[InlineExpr; 2]>,
}
#[derive(Debug)]
pub enum BoolOp {
	Less, More,
	Equal, LessEq, MoreEq
}
fn parse_bool_expr(tt: TokenView) -> PRes<InlineExpr> {
	let (mut tt, mut lhs) = parse_arithmetic(tt)?;
	while let Some((rest, op)) = parse_bool_op(tt) {
		let (rest, rhs) = parse_arithmetic(rest)?;
		lhs = InlineExpr::Bool(BoolExpr {
			args: Box::new([lhs, rhs]),
			op,
		});
		tt = rest;
	}
	Some((tt, lhs))
}
fn parse_bool_op(mut tt: TokenView) -> PRes<BoolOp> {
	let op = match tt.peek()?.kind {
		TokenKind::Less => BoolOp::Less,
		TokenKind::More => BoolOp::More,
		TokenKind::EqEq => BoolOp::Equal,
		TokenKind::LessEq => BoolOp::LessEq,
		TokenKind::MoreEq => BoolOp::MoreEq,
		_ => None?
	};
	tt.next();
	Some((tt, op))
}

// Arithmetic = FuncCall ((:plus / :minus) FuncCall)*
#[derive(Debug)]
pub struct Arithmetic {
	op: ArithOp,
	args: Box<[InlineExpr; 2]>,
}
#[derive(Debug)]
pub enum ArithOp {
	Add, Sub,
}
fn parse_arithmetic(tt: TokenView) -> PRes<InlineExpr> {
	let (mut tt, mut lhs) = parse_func_call(tt)?;
	while let Some((rest, op)) = parse_arith_op(tt) {
		let (rest, rhs) = parse_func_call(rest)?;
		lhs = InlineExpr::Arith(Arithmetic {
			args: Box::new([lhs, rhs]),
			op,
		});
		tt = rest;
	}
	Some((tt, lhs))
}
fn parse_arith_op(mut tt: TokenView) -> PRes<ArithOp> {
	let op = match tt.peek()?.kind {
		TokenKind::Plus => ArithOp::Add,
		TokenKind::Minus => ArithOp::Sub,
		_ => None?,
	};
	tt.next();
	Some((tt, op))
}

// FuncCall = LitExpr (:paren{List(false, true, Expr, :comma)})?
#[derive(Debug)]
pub struct FuncCall {
	func: Box<InlineExpr>,
	args: Vec<Expr>,
}
fn parse_func_call(tt: TokenView) -> PRes<InlineExpr> {
	let (mut tt, mut func) = parse_lit_expr(tt)?;
	if tt.peek_if(TokenKind::Paren(0)).is_some() {
		let (_, args) = (parse_list::<false, true, _, _, _, _>(
			parse_expr,
			parse_token(TokenKind::Comma),
		))(tt.children()?)?;
		func = InlineExpr::Func(FuncCall {
			func: Box::new(func),
			args,
		});
		tt.next();
	}
	Some((tt, func))
}

// LitExpr = :name / :number / :string
#[derive(Debug)]
pub enum LitExpr {
	Name(usize),
	Number(usize),
	String(usize),
}
fn parse_lit_expr(mut tt: TokenView) -> PRes<InlineExpr> {
	let out = match tt.peek()?.kind {
		TokenKind::Name(i) => LitExpr::Name(i),
		TokenKind::Number(i) => LitExpr::Number(i),
		TokenKind::String(i) => LitExpr::String(i),
		_ => None?
	};
	tt.next();
	Some((tt, InlineExpr::Lit(out)))
}


#[inline]
fn parse_epsilon(tt: TokenView) -> Option<()> {
	if !tt.is_empty() { return None; }
	Some(())
}

#[inline]
fn parse_token(k: TokenKind) -> impl FnMut(TokenView) -> PRes<TokenNode> {
	move |mut tt| {
		let n = tt.expect(k)?;
		Some((tt, n))
	}
}

fn parse_list<
	const TRAILING: bool,
	const EPSILON: bool,
	RuleOut,
	SepOut,
	Rule: FnMut(TokenView) -> PRes<RuleOut>,
	Sep: FnMut(TokenView) -> PRes<SepOut>,
>(
	mut rule: Rule,
	mut sep: Sep
) -> impl FnMut(TokenView) -> PRes<Vec<RuleOut>> {
		move |mut tt: TokenView| {
			let mut out = vec![];
			if TRAILING {
				while !tt.is_empty() {
					let Some((rest, elem)) = rule(tt)
					else { break; };
					out.push(elem);
					tt = rest;
					let Some((rest, _)) = sep(tt)
					else { break; };
					tt = rest;
				}
			} else if let Some((rest, elem)) = rule(tt) {
				out.push(elem);
				tt = rest;
				while !tt.is_empty() {
					let Some((rest, _)) = sep(tt)
					else { break; };
					let Some((rest, elem)) = rule(rest)
					else { break; };
					out.push(elem);
					tt = rest;
				}
			}
			if EPSILON { parse_epsilon(tt)?; }
			Some((tt, out))
		}
}
