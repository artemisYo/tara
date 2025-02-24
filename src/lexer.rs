use crate::Provenance;
use crate::tokens::*;

pub type Lexer<'src> = crate::misc::PeekN<LexerIter<'src>>;

impl<'src> From<&'src str> for Lexer<'src> {
    fn from(value: &'src str) -> Self {
		LexerIter::new(value).into()
    }
}

pub struct LexerIter<'src> {
	source: &'src [u8],
	offset: usize,
}
impl<'src> LexerIter<'src> {
	pub fn new(source: &'src str) -> Self {
		Self {
			source: source.as_bytes(),
			offset: 0
		}
	}
}

impl<'src> Iterator for LexerIter<'src> {
	type Item = Token<'src>;

	fn next(&mut self) -> Option<Self::Item> {
		self.strip_whitespace();
		if self.source.len() <= self.offset { return None;}
		None
			.or_else(|| self.comment())
			.or_else(|| self.immediates())
			.or_else(|| self.keywords())
			.or_else(|| self.bools())
			.or_else(|| self.string())
			.or_else(|| self.number())
			.or_else(|| self.name())
    }
}

impl<'src> LexerIter<'src> {
	const fn mk_tk(t: Tokenkind) -> (&'static [u8], Tokenkind) { (t.spelling().unwrap().as_bytes(), t) }
	const IMMS: &'static [(&'static [u8], Tokenkind)] =  {
		use Tokenkind::*;
		&[
			Self::mk_tk(OpenParen),
			Self::mk_tk(CloseParen),
			Self::mk_tk(OpenBrace),
			Self::mk_tk(CloseBrace),
			Self::mk_tk(OpenBracket),
			Self::mk_tk(CloseBracket),
			Self::mk_tk(Comma),
			Self::mk_tk(Semicolon),
			Self::mk_tk(Colon),
			Self::mk_tk(Slash),
			Self::mk_tk(Ellipsis),
		]
	};
	fn cursor(&self) -> &'src [u8] {
		&self.source[self.offset..]
	}
	fn strip_whitespace(&mut self) {
		let len = self.cursor()
			.into_iter()
			.take_while(|&&b| (b as char).is_ascii_whitespace())
			.count();
		self.offset += len;
	}
	
	fn comment(&mut self) -> Option<Token<'src>> {
		let text = self.cursor();
		let start = self.offset;
		if !text.starts_with(b"//") {
			return None;
		}
		let len = text
			.into_iter()
			.take_while(|&&b| b != b'\n')
			.count()
			+ 1;
		self.offset += len;
		Some(Token {
			kind: Tokenkind::Comment,
			loc: Provenance {
				start,
				end: self.offset,
				source: std::str::from_utf8(self.source).unwrap(),
			},
		})
	}

	fn from_table(&mut self, t: &[(&[u8], Tokenkind)]) -> Option<Token<'src>> {
		let text = self.cursor();
		let start = self.offset;
		for (s, k) in t {
			if !text.starts_with(s) {
				continue;
			}
			self.offset += s.len();
			return Some(Token {
				loc: Provenance {
					start,
					end: self.offset,
					source: std::str::from_utf8(self.source).unwrap()
				},
				kind: *k,
			});
		}
		None
	}
	
	fn immediates(&mut self) -> Option<Token<'src>> {
		self.from_table(Self::IMMS)
	}
	
	fn keywords(&mut self) -> Option<Token<'src>> {
		use Tokenkind::*;
		const TABLE: &[(&[u8], Tokenkind)] = &[
			LexerIter::mk_tk(Func),
			LexerIter::mk_tk(Return),
			LexerIter::mk_tk(Loop),
			LexerIter::mk_tk(Break),
			LexerIter::mk_tk(If),
			LexerIter::mk_tk(Else),
			LexerIter::mk_tk(Let),
			LexerIter::mk_tk(Mut),
			LexerIter::mk_tk(Type),
			LexerIter::mk_tk(As),
			LexerIter::mk_tk(Operator),
			LexerIter::mk_tk(Equals),
			LexerIter::mk_tk(Import)
		];
		self.from_table(TABLE)
	}
	
	fn bools(&mut self) -> Option<Token<'src>> {
		use Tokenkind::*;
		const TABLE: &[(&[u8], Tokenkind)] = &[
			(b"true", Bool),
			(b"false", Bool)
		];
		self.from_table(TABLE)
	}
	
	fn string(&mut self) -> Option<Token<'src>> {
		let text = self.cursor();
		let start = self.offset;
		if text[0] != b'"' { return None; }
		let mut escaped = true;
		let mut len = 0;
		for &c in text {
			len += 1;
			match c {
				b'\\' => escaped = !escaped,
				b'\n' => break,
				b'"' if !escaped => break,
				_ => escaped = false,
			}
		}
		self.offset += len;
		Some(Token {
			kind: Tokenkind::String,
			loc: Provenance {
				start,
				end: self.offset,
				source: std::str::from_utf8(self.source).unwrap()
			},
		})
	}
	
	fn number(&mut self) -> Option<Token<'src>> {
		let text = self.cursor();
		let start = self.offset;
		let len = text
			.into_iter()
			.take_while(|&&b| (b as char).is_ascii_digit())
			.count();
		self.offset += len;
		if len == 0 { return None; }
		Some(Token {
			kind: Tokenkind::Number,
			loc: Provenance {
				start,
				end: self.offset,
				source: std::str::from_utf8(self.source).unwrap()
			},
		})
	}
	
	fn name(&mut self) -> Option<Token<'src>> {
		let text = self.cursor();
		let start = self.offset;
		let len = text
			.into_iter()
			.take_while(|&&b| !is_word_break(b) || (b as char).is_ascii_digit())
			.count();
		self.offset += len;
		if len == 0 { return None; }
		Some(Token {
			kind: Tokenkind::Name,
			loc: Provenance {
				start,
				end: self.offset,
				source: std::str::from_utf8(self.source).unwrap()
			},
		})
	}
}

fn is_word_break(b: u8) -> bool {
	if (b as char).is_ascii_digit() { return true; }
	if (b as char).is_ascii_whitespace() { return true; }
	if b == b'"' { return true; }
	LexerIter::IMMS.into_iter().any(|(s, _)| s.starts_with(&[b]))
}
