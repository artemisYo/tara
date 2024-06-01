use crate::errors::{ErrorQueue, Message};

#[derive(Debug)]
pub struct TokenTree {
	tokens: Vec<TokenNode>,
	source: String,
}
#[derive(Debug)]
pub struct TokenNode {
	cc: usize,
	kind: TokenKind,
	span: Span,
}
#[derive(Debug, Clone, Copy)]
pub struct Span {
	start: usize,
	end: usize,
}
#[derive(Debug, Clone, Copy)]
pub enum TokenKind {
	Plus,
	Minus,
	Func,
	Ident,
	Number,
	Paren,
	Brace,
	Bracket,
}

pub fn tokenize(errs: &mut ErrorQueue, string: String) -> Option<TokenTree> {
	let stream = SplitIntersperse::new(&string, on_delims)
		.filter(|span| !string[span.start..span.end].trim().is_empty())
		.peekable();
	let mut tokenizer = Tokenizer {
		buffer: vec![],
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
		source: string,
	})
}

struct Tokenizer<'a, S: Iterator> {
	errs: &'a mut ErrorQueue,
	buffer: Vec<TokenNode>,
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
	fn run_toplevel(&mut self) -> TokRes {
		let tokenizers = &[
			Self::run_keys,
			Self::run_puncts,
			Self::run_groups,
			Self::run_numbers,
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
	
	fn run_ident(&mut self) -> TokRes {
		let span = self.stream.next().unwrap();
		self.buffer.push(TokenNode {
			cc: 0,
			kind: TokenKind::Ident,
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
					cc: 0,
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
					cc: 0,
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
			self.buffer.push(TokenNode {
				cc: 0,
				kind: TokenKind::Number,
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
				cc: 0,
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
			self.buffer[idx].cc = cc;
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

fn on_delims(s: &str) -> Option<usize> {
	let w = s.chars().take_while(|c| c.is_whitespace()).count();
	if w != 0 { return Some(w); }
	
	for d in DELIMS {
		if s.starts_with(d) { return Some(d.chars().count()); }
	}
	None
}

macro_rules! mk_lookup {
	(
		Keys: [ $($ks:literal : $kt:path),* $(,)? ],
		Puncts: [ $($ps:literal : $pt:path),* $(,)? ],
		Groups: [ $($go:literal , $gc:literal : $gt:path),* $(,)? ],
	) => {
		const KEYS: &[(&str, TokenKind)] = &[ $( ($ks, $kt), )* ];
		const PUNCTS: &[(&str, TokenKind)] = &[ $( ($ps, $pt), )* ];
		const GROUPS: &[(&str, &str, TokenKind)] = &[ $( ($go, $gc, $gt), )* ];
		const DELIMS: &[&str] = &[ $( $ps, )* $( $go, )* $( $gc, )* ];
	};
}

mk_lookup! {
	Keys: [ "func": TokenKind::Func ],
	Puncts: [
		"+": TokenKind::Plus,
		"-": TokenKind::Minus,
	],
	Groups: [
		"(", ")": TokenKind::Paren,
		"{", "}": TokenKind::Brace,
		"[", "]": TokenKind::Bracket,
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

struct SplitIntersperse<'a> {
	chars: std::str::Chars<'a>,
	needle: fn(&str) -> Option<usize>,
	gap_start: usize,
	gap_end: usize,
}

impl<'a> Iterator for SplitIntersperse<'a> {
	type Item = Span;
	fn next(&mut self) -> Option<Self::Item> {
		if self.has_gap() {
			return Some(self.extract_gap());
		}
		if let Some(len) = self.find_next_match() {
			let out = self.extract_gap();
			self.gap_end += len;
			return Some(out);
		}
		if self.has_gap() {
			return Some(self.extract_gap());
		}
		None
	}
}

impl<'a> SplitIntersperse<'a> {
	fn new(string: &'a str, needle: fn(&str) -> Option<usize>) -> Self {
		Self {
			chars: string.chars(),
			needle,
			gap_start: 0,
			gap_end: 0,
		}
	}
	fn extract_gap(&mut self) -> Span {
		let out = Span {
			start: self.gap_start,
			end: self.gap_end,
		};
		self.gap_start = self.gap_end;
		return out;
	}
	fn find_next_match(&mut self) -> Option<usize> {
		while !self.is_empty() {
			let Some(count) = (self.needle)(self.chars.as_str())
			else {
				let c = self.chars.next().unwrap();
				self.gap_end += c.len_utf8();
				continue;
			};
			let mut len = 0;
			for _ in 0..count {
				let c = self.chars.next().unwrap();
				len += c.len_utf8();
			}
			return Some(len);
		}
		None
	}
	#[inline]
	fn has_gap(&self) -> bool {
		self.gap_start < self.gap_end
	}
	#[inline]
	fn is_empty(&self) -> bool {
		self.chars.as_str().is_empty()
	}
}
