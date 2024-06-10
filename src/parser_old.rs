use crate::tokenizer::{TokenKind, TokenView, TokenNode};

type PRes<'a, T> = Option<(TokenView<'a>, T)>;

pub fn parse(tt: TokenView) -> Option<File> {
	parse_file(tt).map(|(_, f)| f)
}
	
// File = (&:func Func)* ε
#[derive(Debug)]
pub struct File {
	funcs: Vec<Func>,
}
fn parse_file(mut tt: TokenView) -> PRes<File> {
	let mut funcs = vec![];
	while !tt.is_empty() {
		match tt.peek().unwrap().kind {
			TokenKind::Func => {
				let Some((rest, func)) = parse_func(tt)
				else { break; };
				funcs.push(func);
				tt = rest;
			},
			_ => {}
		}
	}
	if !tt.is_empty() { return None; }
	Some((tt, File { funcs }))
}

// Func = :func :name :paren{Args} (:arrRight :name)? :brace{_}
#[derive(Debug)]
pub struct Func {
	name: usize,
	ret: Option<usize>,
	args: Args,
	body: Block,
}
fn parse_func(mut tt: TokenView) -> PRes<Func> {
	let _func = tt.expect(TokenKind::Func)?;
	let name = tt.expect(TokenKind::Name(0))?.string_idx()?;
	let (_, args) = parse_args(tt.children_if(TokenKind::Paren(0))?)?;
	tt.next();
	let ret = if tt.peek()?.kind == TokenKind::ArrRight {
		tt.next();
		Some(tt.expect(TokenKind::Name(0))?.string_idx()?)
	} else {
		None
	};
	let (_, body) = parse_block(tt.children_if(TokenKind::Brace(0))?)?;
	tt.next();
	Some((tt, Func { name, ret, args, body }))
}

// Args = List(Decl, :comma, trailing = true, expect-ε = true)
#[derive(Debug)]
pub struct Args {
	decls: Vec<Decl>
}
fn parse_args(tt: TokenView) -> PRes<Args> {
	let (tt, decls) =
		(parse_list::<true, true, _, _, _, _>(
			parse_decl,
			parse_token(TokenKind::Comma)
		))(tt)?;
	Some((tt, Args { decls }))
}

// Decl = :name :colon :name
#[derive(Debug)]
pub struct Decl {
	name: usize,
	typ: usize,
}
fn parse_decl(mut tt: TokenView) -> PRes<Decl> {
	let name = tt.expect(TokenKind::Name(0))?.string_idx()?;
	tt.expect(TokenKind::Colon)?;
	let typ = tt.expect(TokenKind::Name(0))?.string_idx()?;
	Some((tt, Decl { name, typ }))
}

// Block = (Statement :semi)* Expr? ε
#[derive(Debug)]
pub struct Block {
	statements: Vec<Statement>,
	tail: Option<Expr>,
}
fn parse_block(mut tt: TokenView) -> PRes<Block> {
	let mut statements = vec![];
	while !tt.is_empty() {
		let Some((mut rest, statement)) = parse_statement(tt)
		else { break; };
		let Some(_) = rest.expect(TokenKind::Semi)
		else { break; };
		statements.push(statement);
		tt = rest;
	}
	let tail = if let Some((rest, expr)) = parse_expr(tt) {
		tt = rest;
		Some(expr)
	} else {
		None
	};
	parse_epsilon(tt)?;
	Some((tt, Block { statements, tail }))
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
