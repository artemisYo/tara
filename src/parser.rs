use crate::tokenizer::{TokenKind, TokenView};

pub struct File {
	strings: HashMap<usize, Box<str>>,
	funcs: Vec<Func>,
}

pub struct Func {
	name: usize,
	ret: Option<usize>,
	args: Args,
}

pub struct Args {
	
}

pub fn parse(tt: TokenView) {
}

struct Parser {
	// maybe instead use a Vec with lifetime based permissions?
	strings: HashMap<usize, Box<str>>,
}

impl Parser {
	// File <- (:func & Function)*
	fn parse_file(&mut self, tt: TokenView) -> Option<File> {
		match tt.peek().unwrap().kind {
			TokenKind::Func => todo!()
			_ => {}
		}
	}
	// Function <- :func :ident :paren{args} (:arrow :ident)?
	fn parse_func(&mut self, tt: TokenView) -> Option<Func> {
		// would be used for creating a span for the ast
		let _func = tt.expect(TokenKind::Func)?;
		let name = tt.expect(TokenKind::Ident)?;
		if tt.peek().unwrap().kind != TokenKind::Paren {
			return None;
		}
		let args = self.parse_args(tt.children()?)?;
		let ret = if tt.peek()?.kind == TokenKind::ArrRight {
			tt.next();
			Some(tt.expect(TokenKind::Ident)?)
		} else {
			None
		};

		self.strings.insert()
		Some(Func { name, ret, args })
	}
	// Args <- (:ident :colon :ident :comma)* (:ident :colon :ident)?
	fn parse_args(&mut self, tt: TokenView) -> Option<Args> {
		todo!()
	}
}
