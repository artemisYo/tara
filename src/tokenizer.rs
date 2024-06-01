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

pub fn tokenize(string: String) -> Option<TokenTree> {
	let stream = SplitIntersperse::new(&string, on_delims)
		.filter(|span| !string[span.start..span.end].trim().is_empty())
		.peekable();
	let mut tokenizer = Tokenizer {
		buffer: vec![],
		string: &string,
		stream,
	};
	if tokenizer.run_toplevel() == TokRes::Fatal {
		return None;
	}
	Some(TokenTree {
		tokens: tokenizer.buffer,
		source: string,
	})
}

struct Tokenizer<'a, S: Iterator> {
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
	fn run_toplevel(&mut self) -> TokRes {
		let tokenizers = &[
			Self::run_keys,
			Self::run_puncts,
			Self::run_groups,
			Self::run_numbers,
			Self::run_ident,
		];

		while let Some(&span) = self.stream.peek() {
			if is_group_closing(&self.string[span.start..span.end]) {
				return TokRes::Match;
			}
			match tokenizers.iter()
				.map(|f| f(self, span))
				.find(|r| *r != TokRes::NoMatch)
			{				
				Some(TokRes::Fatal) => return TokRes::Fatal,
				None => todo!("report wholly unrecognized token"),
				_ => {}
			}
			self.stream.next();
		}
		TokRes::Match
	}
	
	fn run_ident(&mut self, span: Span) -> TokRes {
		self.buffer.push(TokenNode {
			cc: 0,
			kind: TokenKind::Ident,
			span,
		});
		TokRes::Match
	}
	fn run_keys(&mut self, span: Span) -> TokRes {
		let string = &self.string[span.start..span.end];
		for (key, token) in KEYS {
			if string == *key {
				self.buffer.push(TokenNode {
					cc: 0,
					kind: *token,
					span,
				});
				return TokRes::Match;
			}
		}
		TokRes::NoMatch
	}
	fn run_puncts(&mut self, span: Span) -> TokRes {
		let string = &self.string[span.start..span.end];
		for (punct, token) in PUNCTS {
			if string == *punct {
				self.buffer.push(TokenNode {
					cc: 0,
					kind: *token,
					span
				});
				return TokRes::Match;
			}
		}
		TokRes::NoMatch
	}
	fn run_numbers(&mut self, span: Span) -> TokRes {
		let string = &self.string[span.start..span.end];
		const MODES: &[(&str, u32)] = &[
			("0x", 16),
			("0b", 2),
			("", 10), // default
		];

		if !string.starts_with(|c: char| c.is_digit(10)) {
			return TokRes::NoMatch;
		}

		for (prefix, base) in MODES {
			let Some(rest) = string.strip_prefix(prefix)
			else { continue; };
			if rest.chars().any(|c| !c.is_digit(*base)) {
				todo!("report int token error");
			}
			self.buffer.push(TokenNode {
				cc: 0,
				kind: TokenKind::Number,
				span,
			});
			return TokRes::Match;
		}
		
		unreachable!()
	}
	fn run_groups(&mut self, open_span: Span) -> TokRes {
		let open_string = &self.string[open_span.start..open_span.end];
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

			let Some(close_span) = self.stream.peek()
			else {
				todo!("Report unclosed group");
				return TokRes::Fatal;
			};
			let close_string = &self.string[close_span.start..close_span.end];
			if close_string != *close {
				todo!("Report wrong group closing");
				return TokRes::Fatal;
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
