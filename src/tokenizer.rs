use crate::errors::{ErrorQueue, Message};

#[derive(Debug)]
pub struct TokenTree {
	tokens: Vec<TokenNode>,
	source: Vec<Box<str>>,
}
#[derive(Debug, Clone, Copy)]
pub struct TokenNode {
	pub kind: TokenKind,
	pub span: Span,
}
#[derive(Debug, Clone, Copy)]
pub struct Span {
	start: usize,
	end: usize,
}
#[derive(Debug, Clone, Copy)]
pub enum TokenKind {
	// Keywords / Puncts
	Eq,
	EqEq,
	Less,
	More,
	LessEq,
	MoreEq,
	Plus,
	Minus,
	Semi,
	Colon,
	Comma,
	ArrRight,
	Func,
	Let,
	If,
	Else,
	// Literals(string_idx)
	Name(usize),
	Number(usize),
	String(usize),
	// Groups(child_count)
	Paren(usize),
	Brace(usize),
	Bracket(usize),
}
impl PartialEq for TokenKind {
	fn eq(&self, other: &TokenKind) -> bool {
		std::mem::discriminant(self) == std::mem::discriminant(other)
	}
}
impl Eq for TokenKind {}

#[derive(Debug, Clone, Copy)]
pub struct TokenView<'a> {
	tokens: &'a [TokenNode],
	source: &'a [Box<str>],
}
impl TokenTree {
	pub fn view<'a>(&'a self) -> TokenView<'a> {
		TokenView {
			tokens: &self.tokens,
			source: &self.source,
		}
	}
}
impl TokenNode {
	fn cc(&self) -> usize {
		match self.kind {
			TokenKind::Paren(cc) |
			TokenKind::Brace(cc) |
			TokenKind::Bracket(cc) => cc,
			_ => 0
		}
	}
	fn set_cc(&mut self, cc: usize) {
		self.kind = match self.kind {
			TokenKind::Paren(_) => TokenKind::Paren(cc),
			TokenKind::Brace(_) => TokenKind::Brace(cc),
			TokenKind::Bracket(_) => TokenKind::Bracket(cc),
			t => t,
		};
	}
	pub fn string_idx(&self) -> Option<usize> {
		match self.kind {
			TokenKind::Name(i)   |
			TokenKind::Number(i) |
			TokenKind::String(i) => Some(i),
			_ => None,
		}
	}
}
impl<'a> TokenView<'a> {
	pub fn next(&mut self) -> Option<TokenNode> {
		let cur = *self.tokens.first()?;
		self.tokens = self.tokens.get(cur.cc() + 1..)?;
		Some(cur)
	}
	pub fn peek(&self) -> Option<TokenNode> {
		let cur = *self.tokens.first()?;
		Some(cur)
	}
	pub fn expect(&mut self, k: TokenKind) -> Option<TokenNode> {
		if self.peek()?.kind != k { return None; }
		self.next()
	}
	pub fn peek_if(&self, k: TokenKind) -> Option<TokenNode> {
		self.peek().filter(|t| t.kind == k)
	}
	pub fn children(&self) -> Option<Self> {
		let mut out = *self;
		let cc = self.tokens.first()?.cc();
		out.tokens = &out.tokens[1..][..cc];
		Some(out)
	}
	pub fn children_if(&self, k: TokenKind) -> Option<Self> {
		self.peek_if(k).map(|_| self.children())?
	}
	pub fn get_current_string(&self) -> Option<&str> {
		let cur = self.tokens.first()?.string_idx()?;
		Some(&self.source.get(cur)?)
	}
	pub fn get_string(&self, t: TokenNode) -> Option<&str> {
		Some(&self.source.get(t.string_idx()?)?)
	}
	pub fn is_empty(&self) -> bool {
		self.tokens.is_empty()
	}
}

pub fn tokenize(errs: &mut ErrorQueue, string: &str) -> Option<TokenTree> {
	let stream = ChunksByFn::new(string, split_on_delims)
		.filter(|span| !string[span.start..span.end].trim().is_empty())
		.peekable();
	let mut tokenizer = Tokenizer {
		buffer: vec![],
		source: vec![],
		string: &string,
		stream,
		errs,
	};
	match tokenizer.run_toplevel() {
		TokRes::Fatal => return None,
		TokRes::NoMatch => {
			errs.log_error(TokErr::ClosedUnopenedGroup);
			return None;
		},
		TokRes::Match => {}
	}
	Some(TokenTree {
		tokens: tokenizer.buffer,
		source: tokenizer.source,
	})
}

struct Tokenizer<'a, S: Iterator> {
	errs: &'a mut ErrorQueue,
	buffer: Vec<TokenNode>,
	source: Vec<Box<str>>,
	stream: std::iter::Peekable<S>,
	string: &'a str,
}

#[derive(PartialEq, Eq)]
enum TokRes {
	// includes errors we recovered from
	Match,
	// try next tokenizer
	NoMatch,
	
	Fatal,
}

// tokenizers
impl<'a, S: Iterator<Item = Span>> Tokenizer<'a, S> {
	fn get_string(&self, span: Span) -> &str {
		&self.string[span.start..span.end]
	}
	fn intern_string(&mut self, span: Span) -> usize {
		let string = self.get_string(span);
		match self.source.iter()
			.enumerate()
			.find(|(_, s)| ***s == *string)
			.map(|(i, _)| i)
		{
			Some(i) => i,
			None => {
				let i = self.source.len();
				self.source.push(string.into());
				i
			},
		}
	}
	fn run_toplevel(&mut self) -> TokRes {
		let tokenizers = &[
			Self::run_keys,
			Self::run_puncts,
			Self::run_groups,
			Self::run_numbers,
			Self::run_string,
			Self::run_ident,
		];

		while let Some(&span) = self.stream.peek() {
			if is_group_closing(self.get_string(span)) {
				// indicate expectation of continuation
				return TokRes::NoMatch;
			}
			match tokenizers.iter()
				.map(|f| f(self))
				.find(|r| *r != TokRes::NoMatch)
			{				
				Some(TokRes::Fatal) => return TokRes::Fatal,
				None => self.errs.log_error(TokErr::UnknownToken(self.get_string(span).into())),
				_ => {}
			}
		}
		TokRes::Match
	}
	fn run_string(&mut self) -> TokRes {
		let &span = self.stream.peek().unwrap();
		let string = self.get_string(span);
		if !string.starts_with('"') {
			return TokRes::NoMatch;
		}
		self.stream.next();
		let idx = self.intern_string(span);
		self.buffer.push(TokenNode {
			kind: TokenKind::String(idx),
			span,
		});
		TokRes::Match
	}
	fn run_ident(&mut self) -> TokRes {
		let span = self.stream.next().unwrap();
		let idx = self.intern_string(span);
		self.buffer.push(TokenNode {
			kind: TokenKind::Name(idx),
			span,
		});
		TokRes::Match
	}
	fn run_keys(&mut self) -> TokRes {
		let &span = self.stream.peek().unwrap();
		let string = self.get_string(span);
		for (key, token) in KEYS {
			if string == *key {
				self.buffer.push(TokenNode {
					kind: *token,
					span,
				});
				self.stream.next();
				return TokRes::Match;
			}
		}
		TokRes::NoMatch
	}
	fn run_puncts(&mut self) -> TokRes {
		let &span = self.stream.peek().unwrap();
		let string = self.get_string(span);
		for (punct, token) in PUNCTS {
			if string == *punct {
				self.buffer.push(TokenNode {
					kind: *token,
					span
				});
				self.stream.next();
				return TokRes::Match;
			}
		}
		TokRes::NoMatch
	}
	fn run_numbers(&mut self) -> TokRes {
		let &span = self.stream.peek().unwrap();
		let string = self.get_string(span);
		const MODES: &[(&str, u32)] = &[
			("0x", 16),
			("0b", 2),
			("", 10), // default
		];

		if !string.starts_with(|c: char| c.is_digit(10)) {
			return TokRes::NoMatch;
		}

		for &(prefix, base) in MODES {
			let Some(rest) = string.strip_prefix(prefix)
			else { continue; };
			if rest.chars().any(|c| !c.is_digit(base)) {
				self.errs.log_error(TokErr::WrongDigit {
					number: string.into(),
					base,
				});
			}
			let idx = self.intern_string(span);
			self.buffer.push(TokenNode {
				kind: TokenKind::Number(idx),
				span,
			});
			self.stream.next();
			return TokRes::Match;
		}
		
		unreachable!()
	}
	fn run_groups(&mut self) -> TokRes {
		let &open_span = self.stream.peek().unwrap();
		let open_string = self.get_string(open_span);
		for (open, close, token) in GROUPS {
			if open_string != *open {
				continue;
			}
			let idx = self.buffer.len();
			self.buffer.push(TokenNode {
				kind: *token,
				span: open_span,
			});
			self.stream.next();
			
			if self.run_toplevel() == TokRes::Fatal {
				return TokRes::Fatal;
			}

			let Some(&close_span) = self.stream.peek()
			else {
				self.errs.log_error(TokErr::EofInScope);
				return TokRes::Fatal;
			};
			let close_string = self.get_string(close_span);
			if close_string != *close {
				self.errs.log_error(TokErr::WrongGroupClosed {
					expected: close,
					found: close_string.into(),
				});
			} else {
				self.stream.next();
			}
			
			let cc = self.buffer.len() - idx - 1;
			self.buffer[idx].set_cc(cc);
			self.buffer[idx].span.end = close_span.end;
			return TokRes::Match;
		}
		TokRes::NoMatch
	}
}

fn is_group_closing(string: &str) -> bool {
	for (_, closing, _) in GROUPS {
		if string == *closing { return true; }
	}
	false
}

macro_rules! mk_lookup {
	(
		Keys: [ $($ks:literal : $kt:path),* $(,)? ],
		Puncts: [ $($ps:literal : $pt:path),* $(,)? ],
		Groups: [ $($go:literal , $gc:literal : $gt:expr),* $(,)? ],
	) => {
		const KEYS: &[(&str, TokenKind)] = &[ $( ($ks, $kt), )* ];
		const PUNCTS: &[(&str, TokenKind)] = &[ $( ($ps, $pt), )* ];
		const GROUPS: &[(&str, &str, TokenKind)] = &[ $( ($go, $gc, $gt), )* ];
		const DELIMS: &[&str] = &[ $( $ps, )* $( $go, )* $( $gc, )* ];
	};
}


mk_lookup! {
	Keys: [
		"func": TokenKind::Func,
		"let": TokenKind::Let,
		"if": TokenKind::If,
		"else": TokenKind::Else,
	],
	// do order from prefix-overlapping strings
	// from longest to shortest
	Puncts: [
		"->": TokenKind::ArrRight,
		"==": TokenKind::EqEq,
		"=": TokenKind::Eq,
		"<=": TokenKind::LessEq,
		">=": TokenKind::MoreEq,
		"<": TokenKind::Less,
		">": TokenKind::More,
		"+": TokenKind::Plus,
		"-": TokenKind::Minus,
		":": TokenKind::Colon,
		";": TokenKind::Semi,
		",": TokenKind::Comma,
	],
	Groups: [
		"(", ")": TokenKind::Paren(0),
		"{", "}": TokenKind::Brace(0),
		"[", "]": TokenKind::Bracket(0),
	],
}

enum TokErr {
	ClosedUnopenedGroup,
	EofInScope,
	UnknownToken(Box<str>),
	WrongDigit {
		number: Box<str>,
		base: u32,
	},
	WrongGroupClosed {
		expected: &'static str,
		found: Box<str>,
	},
}
impl Message for TokErr {
	fn summary(&mut self) -> Box<str> {
		match self {
			Self::ClosedUnopenedGroup => "Found scope closing token outside of any scope!".into(),
			Self::EofInScope => "Reached end of file in unclosed scope!".into(),
			Self::UnknownToken(t) => format!("Unknown token encountered: '{}'", t).into(),
			Self::WrongDigit {
				number,
				base,
			} => format!("Encountered unexpected digit(s) for base {} in number: '{}'", base, number).into(),
			Self::WrongGroupClosed {
				expected,
				found,
			} => format!("Encountered scope closing '{}' when expected '{}'!", found, expected).into(),
		}
	}
	fn hints(&mut self) -> Box<dyn Iterator<Item = Box<str>>> {
		Box::new([].into_iter())
	}
	fn location(&mut self) -> Box<str> {
		"".into()
	}
}

fn split_on_delims(s: &str) -> usize {
	for i in 0..s.len() {
		let s = &s[i..];
		let t = s.trim_start();
		if t.len() != s.len() {
			if i != 0 { return i; }
			return s.len() - t.len();
		}
		if let Some(t) = s.strip_prefix('"') {
			if i != 0 { return i; }
			let mut len = 0;
			let mut escaped = false;
			for c in t.chars() {
				match c {
					'"' if !escaped => return len + 2,
					'\\' if !escaped => escaped = true,
					_ => escaped = false,
				}
				len += c.len_utf8();
			}
			return len + 1;
		}
		for d in DELIMS {
			if s.starts_with(d) {
				if i != 0 { return i; }
				return d.len();
			}
		}
	}
	s.len()
}
struct ChunksByFn<'a> {
	func: fn(&str) -> usize,
	string: &'a str,
	offset: usize,
}
impl<'a> Iterator for ChunksByFn<'a> {
	type Item = Span;
	fn next(&mut self) -> Option<Self::Item> {
		if self.string[self.offset..].is_empty() {
			return None;
		}
		let len = (self.func)(&self.string[self.offset..]);
		let start = self.offset;
		self.offset += len;
		Some(Span {
			end: self.offset,
			start,
		})
	}
}
impl<'a> ChunksByFn<'a> {
	fn new(string: &'a str, func: fn(&str) -> usize) -> Self {
		Self {
			func,
			string,
			offset: 0,
		}
	}
}
