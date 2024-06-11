use std::marker::PhantomData;
use super::Parser;
use crate::tokenizer::TokenView;

pub trait BoolOpt {
	fn opt(self) -> Option<()>;
}
impl BoolOpt for bool {
	fn opt(self) -> Option<()> {
		if self {
			Some(())
		} else  {
			None
		}
	}
}

#[derive(Debug)]
pub struct File(pub Vec<TreeNode>);

#[derive(Debug, Default)]
pub struct TreeNode {
	size: usize,
	kind: TreeKind
}

#[derive(Debug, Default)]
pub enum TreeKind {
	Vec,
	Pair,
	Func,
	Block,
	Let,
	IfExpr,
	BiopExpr(Op),
	Funccall,
	Name(usize),
	Number(usize),
	TreeString(usize),
	#[default]
	Void,
}
impl PartialEq for TreeKind {
	fn eq(&self, other: &TreeKind) -> bool {
		std::mem::discriminant(self) == std::mem::discriminant(other)
	}
}
impl Eq for TreeKind {}
impl TreeKind {
	fn is_statement(&self) -> bool {
		*self == Self::Let || self.is_expr()
	}
	fn is_expr(&self) -> bool {
		match self {
			Self::IfExpr |
			Self::BiopExpr(_) |
			Self::Funccall |
			Self::Name(_) |
			Self::Number(_) |
			Self::TreeString(_) => true,
			_ => false
		}
	}
}

#[derive(Debug, Clone, Copy)]
pub enum Op {
	Less, More, Equal,
	LessEq, MoreEq,
	Add, Sub,
}

pub type FileView<'a> = VecView<'a, Func<'a>>;
impl File {
	pub fn view<'a>(&'a self) -> FileView<'a> {
		View::new_at(&self.0)
	}
}

trait ViewBase {
	fn is_valid(_: &[TreeNode]) -> bool;
}
pub trait ValidatorBase {
	type Args;
	fn new() -> Self;
}

pub struct View<'a, T>(&'a [TreeNode], PhantomData<T>);
impl<'a, T: ViewBase> View<'a, T> {
	fn new_at(slice: &'a [TreeNode]) -> Self {
		T::is_valid(slice).opt().unwrap();
		Self(slice, PhantomData)
	}
	fn is_valid(&self) -> bool {
		T::is_valid(self.0)
	}
	pub fn cast<V: ViewBase>(&self) -> Option<View<'a, V>> {
		let out = View::new_at(self.0);
		out.is_valid().opt()?;
		Some(out)
	}
}

// this is simple and good
// I'm scared of the new tlist!
//macro_rules! tlist {
//	() => { () };
//	( $head:tt $(,)? ) => { ($head, ()) };
//	( $head:tt , $($tail:tt),+ $(,)? ) => {
//		($head, tlist!($($tail),+) )
//	};
//}
macro_rules! tlist {
	() => { () };
	( $head:tt $(,)?) => { ( $head, () ) };
	( $head:tt $(| $alt:tt)+ $(,)?) => {
	    ( 
	      tlist!($head, $($alt),+ ), 
	      ()
	    )
	};
	( $head:tt , $($tail:tt $(| $alt_tail:tt)*),+ $(,)?) => {
	    ( 
	      $head,
	      tlist!($($tail $(| $alt_tail)*),+) 
	    )
	};
	( $head:tt $(| $alt:tt)+ , $($tail:tt $(| $alt_tail:tt)*),+ $(,)?) => {
	    ( 
	      tlist!($head, $($alt),+ ), 
	      tlist!($($tail $(| $alt_tail)*),+) 
	    )
	};
}

pub trait Naturals {}
type Zero = ();
type Succ<N> = (N, ());
impl Naturals for Zero {}
impl<N: Naturals> Naturals for Succ<N> {}

pub trait Find<T, N: Naturals> {}
impl<T> Find<T, Zero> for T {}
impl<T, U> Find<T, Zero> for (T, U) {}
impl<F, N, T, U> Find<F, Succ<N>> for (T, U)
where N: Naturals,
      U: Find<F, N> {}

type Nil = ();
pub struct Validator<T, Args>(PhantomData<(T, Args)>);
impl<T, Head, Tail> Validator<T, (Head, Tail)> {
	pub fn apply<H, N: Naturals>(self, _: H) -> Validator<T, Tail>
	where Head: Find<H, N> {
		Validator(PhantomData)
	}
}
impl<T: ValidatorBase> Validator<T, T::Args> {
	pub fn new() -> Self {
		Validator(PhantomData)
	}
}
impl<T> Validator<T, ()>
where T: ValidatorBase {
	pub fn finish(self) -> T {
		T::new()
	}
}

pub struct Scope<T, Args> {
	index: usize,
	validator: Validator<T, Args>
}
impl<T, Head, Tail> Scope<T, (Head, Tail)> {
	fn validate<P, N: Naturals>(self, p: P) -> Scope<T, Tail>
	where Head: Find<P, N> {
		let index = self.index;
		let validator = self.validator.apply(p);
		Scope { index, validator }
	}
}
impl Parser {
	pub fn push_name(&mut self, idx: usize) -> NameBase {
		self.buffer.push(TreeNode {
			size: 1, kind: TreeKind::Name(idx),
		});
		NameBase
	}
	pub fn push_void(&mut self) -> VoidBase {
		self.buffer.push(TreeNode {
			size: 1, kind: TreeKind::Void,
		});
		VoidBase
	}
	pub fn new_scope<T: ValidatorBase>(&mut self) -> Scope<T, T::Args> {
		let index = self.buffer.len();
		self.buffer.push(TreeNode::default());
		Scope {
			validator: Validator::new(),
			index,
		}
	}
	pub fn close_scope<T: ValidatorBase>(
		&mut self,
		scope: Scope<T, ()>,
		kind: TreeKind
	) -> T {
		let size = self.buffer.len() - scope.index;
		self.buffer[scope.index].size = size;
		self.buffer[scope.index].kind = kind;
		scope.validator.finish()
	}
	pub fn with_scope<'a, T, P, Head, Tail, N: Naturals>(
		&mut self,
		scope: Scope<T, (Head, Tail)>,
		f: impl FnOnce(&mut Self, TokenView<'a>) -> Option<(TokenView<'a>, P)>,
		tt: TokenView<'a>,
	) -> Option<(TokenView<'a>, Scope<T, Tail>)>
	where Head: Find<P, N> {
		let (t, p) = f(self, tt)?;
		let s = scope.validate(p);
		Some((t, s))
	}
}

pub struct VecBase<T>(PhantomData<T>);
pub type VecView<'a, T> = View<'a, VecBase<T>>;
impl<T> ViewBase for VecBase<T> {
	fn is_valid(slice: &[TreeNode]) -> bool {
		slice[0].kind == TreeKind::Vec
	}
}
impl<T> ValidatorBase for VecBase<T> {
	type Args = ();
	fn new() -> Self { Self(PhantomData) }
}
impl<'a, T> VecView<'a, T> {
	fn in_bounds(&self, offset: usize) -> bool {
		offset < self.0[0].size
	}
	fn get_offset(&self, idx: usize) -> Option<&'a [TreeNode]> {
		let mut offset = 1;
		for _ in 0..idx {
			self.in_bounds(offset).opt()?;
			let cur_node = &self.0[offset];
			offset += cur_node.size;
		}
		self.in_bounds(offset).opt()?;
		Some(&self.0[offset..])
	}
	// idx refers to the node number
	// not to the idx in &[TreeNode]
	pub fn get(&self, idx: usize) -> Option<View<'a, T>>
	where T: ViewBase {
		Some(View::new_at(self.get_offset(idx)?))
	}
}

pub struct PairBase<T, U>(PhantomData<(T, U)>);
pub type Pair<'a, T, U> = View<'a, PairBase<T, U>>;
impl<T, U> ViewBase for PairBase<T, U> {
	fn is_valid(slice: &[TreeNode]) -> bool {
		slice[0].kind == TreeKind::Pair
	}
}
impl<T, U> ValidatorBase for PairBase<T, U> {
	type Args = tlist!(T, U);
	fn new() -> Self { Self(PhantomData) }
}
impl<'a, T, U> Pair<'a, T, U> {
	pub fn first(&self) -> View<'a, T> where T: ViewBase {
		View::new_at(&self.0[1..])
	}
	fn second_offset(&self) -> &'a [TreeNode] {
		&self.0[1 + self.0[1].size..]
	}
	pub fn second(&self) -> View<'a, U> where U: ViewBase {
		View::new_at(self.second_offset())
	}
}

pub type ParamsBase = VecBase<PairBase<NameBase, NameBase>>;
pub type Params<'a> = VecView<'a, Pair<'a, Name<'a>, Name<'a>>>;
pub struct FuncBase;
pub type Func<'a> = View<'a, FuncBase>;
impl ViewBase for FuncBase {
	fn is_valid(slice: &[TreeNode]) -> bool {
		slice[0].kind == TreeKind::Func
	}
}
impl ValidatorBase for FuncBase {
	type Args = tlist!(
		NameBase,
		ParamsBase,
		NameBase | VoidBase,
		BlockBase,
	);
	fn new() -> Self { Self }
}
impl<'a> Func<'a> {
	pub fn name(&self) -> Name<'a> {
		View::new_at(&self.0[1..])
	}
	pub fn params(&self) -> Params<'a> {
		View::new_at(&self.0[2..])
	}
	fn ret_offset(&self) -> &'a [TreeNode] {
		&self.0[2 + self.0[2].size..]
	}
	pub fn ret(&self) -> Option<Name<'a>> {
		let ret = self.ret_offset();
		(ret[0].kind != TreeKind::Void).opt()?;
		Some(View::new_at(ret))
	}
	fn body_offset(&self) -> &'a [TreeNode] {
		let ret = self.ret_offset();
		&ret[ret[0].size..]
	}
	pub fn body(&self) -> Block<'a> {
		View::new_at(self.body_offset())
	}
}

pub type StatementsBase = VecBase<StatementBase>;
pub type Statements<'a> = VecView<'a, Statement<'a>>;
pub struct BlockBase;
pub type Block<'a> = View<'a, BlockBase>;
impl ViewBase for BlockBase {
	fn is_valid(slice: &[TreeNode]) -> bool {
		slice[0].kind == TreeKind::Block
	}
}
impl ValidatorBase for BlockBase {
	type Args = tlist!(StatementsBase, ExprBase | VoidBase);
	fn new() -> Self { Self }
}
impl<'a> Block<'a> {
	pub fn stmts(&self) -> Statements<'a> {
		View::new_at(&self.0[1..])
	}
	fn tail_offset(&self) -> &'a [TreeNode] {
		&self.0[1 + self.0[1].size..]
	}
	pub fn tail(&self) -> Option<Expr<'a>> {
		let tail = self.tail_offset();
		(tail[0].kind != TreeKind::Void).opt()?;
		Some(View::new_at(tail))
	}
}

pub struct StatementBase;
pub type Statement<'a> = View<'a, StatementBase>;
impl ViewBase for StatementBase {
	fn is_valid(slice: &[TreeNode]) -> bool {
		slice[0].kind.is_statement()
	}
}
impl ValidatorBase for StatementBase {
	type Args = tlist!(ExprBase | LetBase);
	fn new() -> Self { Self }
} 

pub struct ExprBase;
pub type Expr<'a> = View<'a, ExprBase>;
impl ViewBase for ExprBase {
	fn is_valid(slice: &[TreeNode]) -> bool {
		slice[0].kind.is_expr()
	}
}
impl ValidatorBase for ExprBase {
	type Args = tlist!(
		NameBase |
		NumberBase |
		IfExprBase |
		FunccallBase |
		BiopExprBase |
		TreeStringBase
	);
	fn new() -> Self { Self }
}

pub struct LetBase;
pub type Let<'a> = View<'a, LetBase>;
impl ViewBase for LetBase {
	fn is_valid(slice: &[TreeNode]) -> bool {
		slice[0].kind == TreeKind::Let
	}
}
impl ValidatorBase for LetBase {
	type Args = tlist!(NameBase, ExprBase);
	fn new() -> Self { Self }
}
impl<'a> Let<'a> {
	pub fn name(&self) -> Name<'a> {
		View::new_at(&self.0[1..])
	}
	pub fn initializer(&self) -> Expr<'a> {
		View::new_at(&self.0[2..])
	}
}

pub struct IfExprBase;
pub type IfExpr<'a> = View<'a, IfExprBase>;
impl ViewBase for IfExprBase {
	fn is_valid(slice: &[TreeNode]) -> bool {
		slice[0].kind == TreeKind::IfExpr
	}
}
impl ValidatorBase for IfExprBase {
	type Args = tlist!(
		ExprBase,
		ExprBase,
		ExprBase | VoidBase
	);
	fn new() -> Self { Self }
}
impl<'a> IfExpr<'a> {
	pub fn cond(&self) -> Expr<'a> {
		View::new_at(&self.0[1..])
	}
	fn then_offset(&self) -> &'a [TreeNode] {
		&self.0[1 + self.0[1].size..]
	}
	pub fn then(&self) -> Expr<'a> {
		View::new_at(self.then_offset())
	}
	fn else_offset(&self) -> &'a [TreeNode] {
		let then = self.then_offset();
		&then[then[0].size..]
	}
	pub fn else_(&self) -> Option<Expr<'a>> {
		let else_ = self.else_offset();
		(else_[0].kind != TreeKind::Void).opt()?;
		Some(View::new_at(else_))
	}
}

pub struct BiopExprBase;
pub type BiopExpr<'a> = View<'a, BiopExprBase>;
impl ViewBase for BiopExprBase {
	fn is_valid(slice: &[TreeNode]) -> bool {
		slice[0].kind == TreeKind::BiopExpr(Op::Less)
	}
}
impl ValidatorBase for BiopExprBase {
	type Args = tlist!(ExprBase, ExprBase);
	fn new() -> Self { Self }
}
impl<'a> BiopExpr<'a> {
	pub fn op(&self) -> Op {
		match self.0[0].kind {
			TreeKind::BiopExpr(o) => o,
			_ => unreachable!(),
		}
	}
	pub fn rhs(&self) -> Expr<'a> {
		View::new_at(&self.0[1..])
	}
	fn lhs_offset(&self) -> &'a [TreeNode] {
		&self.0[1 + self.0[1].size..]
	}
	pub fn lhs(&self) -> Expr<'a> {
		View::new_at(self.lhs_offset())
	}
}

pub type ArgsBase = VecBase<ExprBase>;
pub type Args<'a> = VecView<'a, Expr<'a>>;
pub struct FunccallBase;
pub type Funccall<'a> = View<'a, FunccallBase>;
impl<'a> Funccall<'a> {
	pub fn base(&self) -> Expr<'a> {
		View::new_at(&self.0[1..])
	}
	fn args_offset(&self) -> &'a [TreeNode] {
		&self.0[1 + self.0[1].size..]
	}
	pub fn args(&self) -> Args<'a> {
		View::new_at(self.args_offset())
	}
}
impl ViewBase for FunccallBase {
	fn is_valid(slice: &[TreeNode]) -> bool {
		slice[0].kind == TreeKind::Funccall
	}
}
impl ValidatorBase for FunccallBase {
	type Args = tlist!(ExprBase, ArgsBase);
	fn new() -> Self { Self }
}

pub struct NameBase;
pub type Name<'a> = View<'a, NameBase>;
impl ViewBase for NameBase {
	fn is_valid(slice: &[TreeNode]) -> bool {
		slice[0].kind == TreeKind::Name(0)
	}
}
impl ValidatorBase for NameBase {
	type Args = ();
	fn new() -> Self { Self }
}
pub struct NumberBase;
pub type Number<'a> = View<'a, NumberBase>;
impl ViewBase for NumberBase {
	fn is_valid(slice: &[TreeNode]) -> bool {
		slice[0].kind == TreeKind::Number(0)
	}
}
impl ValidatorBase for NumberBase {
	type Args = ();
	fn new() -> Self { Self }
}
pub struct TreeStringBase;
pub type TreeString<'a> = View<'a, TreeStringBase>;
impl ViewBase for TreeStringBase {
	fn is_valid(slice: &[TreeNode]) -> bool {
		slice[0].kind == TreeKind::TreeString(0)
	}
}
impl ValidatorBase for TreeStringBase {
	type Args = ();
	fn new() -> Self { Self }
}

pub struct VoidBase;
