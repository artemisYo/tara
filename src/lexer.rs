use std::collections::HashSet;
use std::io::Read;

use either::Either::{self, Left, Right};

use crate::data::files;
use crate::misc::CharRead;
use crate::tokens::*;
use crate::Provenance;

struct Res<O> {
    consume: bool,
    result: Option<O>,
}
impl<O> Res<O> {
    const FAIL: Res<O> = Res {
        consume: false,
        result: None,
    };
    const CONSUME: Res<O> = Res {
        consume: true,
        result: None,
    };
}

trait Lexer<I> {
    type Out;
    fn step(&mut self, input: I) -> Res<Self::Out>;

    fn par<R>(self, other: R) -> Par<Self, R>
    where
        Self: Sized,
    {
        Par(self, other)
    }
    fn confirm<P: FnMut(I) -> bool>(self, pred: P) -> Confirm<Self, Self::Out, P>
    where
        Self: Sized,
    {
        Confirm(self, None, pred)
    }
    fn preconfirm<P: FnOnce(I) -> bool>(self, pred: P) -> Preconfirm<Self, P>
    where
        Self: Sized,
    {
        Preconfirm(self, Left(pred))
    }
}

// runs lexers in parallel
struct Par<L, R>(L, R);
impl<I: Clone, O, L: Lexer<I, Out = O>, R: Lexer<I, Out = O>> Lexer<I> for Par<L, R> {
    type Out = O;
    fn step(&mut self, input: I) -> Res<Self::Out> {
        let l = self.0.step(input.clone());
        let r = self.1.step(input);
        if let Some(result) = l.result {
            Res {
                consume: l.consume,
                result: Some(result),
            }
        } else if let Some(result) = r.result {
            Res {
                consume: r.consume,
                result: Some(result),
            }
        } else {
            Res {
                consume: l.consume || r.consume,
                result: None,
            }
        }
    }
}

// guards a Res::done with a predicate
struct Confirm<L, O, P>(L, Option<O>, P);
impl<I, L: Lexer<I>, P: FnMut(I) -> bool> Lexer<I> for Confirm<L, L::Out, P> {
    type Out = L::Out;

    fn step(&mut self, input: I) -> Res<Self::Out> {
        if let Some(o) = self.1.take() {
            return Res {
                consume: false,
                result: Some(o).filter(|_| self.2(input)),
            };
        }
        let mut res = self.0.step(input);
        if let Some(o) = res.result.take() {
            self.1 = Some(o);
        }
        res
    }
}

struct Preconfirm<L, P>(L, Either<P, bool>);
impl<I: Clone, L: Lexer<I>, P: FnOnce(I) -> bool> Lexer<I> for Preconfirm<L, P> {
    type Out = L::Out;

    fn step(&mut self, input: I) -> Res<Self::Out> {
        fn take_left<L, R>(either: &mut Either<L, R>, replace: R) -> Option<L> {
            match either {
                Left(_) => std::mem::replace(either, Right(replace)).left(),
                Right(_) => None,
            }
        }
        if let Some(p) = take_left(&mut self.1, false) {
            self.1 = Right(p(input.clone()));
        }
        if let Right(true) = self.1 {
            self.0.step(input)
        } else {
            Res::FAIL
        }
    }
}

struct Exact {
    pat: &'static str,
    out: Tokenkind,
}
impl Lexer<char> for Exact {
    type Out = Tokenkind;

    fn step(&mut self, c: char) -> Res<Self::Out> {
        // ensure this instance of the lexer continues failing
        // this is done to avoid "func" matching anything like "fuun_ic"
        if !self.pat.starts_with(c) {
            self.pat = "";
            return Res::FAIL;
        }
        self.pat = &self.pat[c.len_utf8()..];
        Res {
            consume: true,
            result: Some(self.out).filter(|_| self.pat.is_empty()),
        }
    }
}

static IMMS: &[Tokenkind] = &[
    Tokenkind::OpenParen,
    Tokenkind::CloseParen,
    Tokenkind::OpenBrace,
    Tokenkind::CloseBrace,
    Tokenkind::OpenBracket,
    Tokenkind::CloseBracket,
    Tokenkind::Comma,
    Tokenkind::Semicolon,
    Tokenkind::Colon,
    Tokenkind::Slash,
    Tokenkind::Ellipsis,
];

impl From<Tokenkind> for Exact {
    fn from(t: Tokenkind) -> Self {
        Exact {
            pat: t.spelling(),
            out: t,
        }
    }
}
fn immediates() -> impl Lexer<char, Out = Tokenkind> {
    use Tokenkind::*;
    Exact::from(OpenParen)
        .par(Exact::from(CloseParen))
        .par(Exact::from(OpenBrace))
        .par(Exact::from(CloseBrace))
        .par(Exact::from(OpenBracket))
        .par(Exact::from(CloseBracket))
        .par(Exact::from(Comma))
        .par(Exact::from(Semicolon))
        .par(Exact::from(Colon))
        .par(Exact::from(Slash).confirm(|c| c != '/'))
        .par(Exact::from(Ellipsis))
}

fn keywords() -> impl Lexer<char, Out = Tokenkind> {
    use Tokenkind::*;
    Exact::from(Case)
        .par(Exact::from(Func))
        .par(Exact::from(Return))
        .par(Exact::from(Loop))
        .par(Exact::from(Break))
        .par(Exact::from(If))
        .par(Exact::from(Else))
        .par(Exact::from(Let))
        .par(Exact::from(Mut))
        .par(Exact::from(Type))
        .par(Exact::from(As))
        .par(Exact::from(Operator))
        .par(Exact::from(Equals))
        .par(Exact::from(Import))
        .par(Exact::from(Underscore))
        .confirm(is_word_break)
}

fn bools() -> impl Lexer<char, Out = Tokenkind> {
    Exact {
        pat: "true",
        out: Tokenkind::Bool,
    }
    .par(Exact {
        pat: "false",
        out: Tokenkind::Bool,
    })
}

type StringLexer = Preconfirm<StringLexerInternal, fn(char) -> bool>;
impl StringLexer {
    fn new() -> Self {
        StringLexerInternal(true).preconfirm(|c| c == '"')
    }
}
struct StringLexerInternal(bool);
impl Lexer<char> for StringLexerInternal {
    type Out = Tokenkind;
    fn step(&mut self, c: char) -> Res<Tokenkind> {
        let escaped = self.0;
        let done = match c {
            '\n' => true,
            '"' if !escaped => true,
            '\\' => {
                self.0 = !escaped;
                false
            }
            _ => {
                self.0 = false;
                false
            }
        };
        Res {
            consume: true,
            result: Some(Tokenkind::String).filter(|_| done),
        }
    }
}

type NumberLexer = Preconfirm<NumberLexerInternal, fn(char) -> bool>;
impl NumberLexer {
    fn new() -> Self {
        NumberLexerInternal.preconfirm(|c| c.is_ascii_digit())
    }
}
struct NumberLexerInternal;
impl Lexer<char> for NumberLexerInternal {
    type Out = Tokenkind;

    fn step(&mut self, c: char) -> Res<Tokenkind> {
        if c.is_ascii_digit() {
            Res::CONSUME
        } else {
            Res {
                consume: false,
                result: Some(Tokenkind::Number),
            }
        }
    }
}

type NameLexer = Preconfirm<NameLexerInternal, fn(char) -> bool>;
impl NameLexer {
    fn new() -> Self {
        NameLexerInternal.preconfirm(|c| !is_word_break(c))
    }
}
struct NameLexerInternal;
impl Lexer<char> for NameLexerInternal {
    type Out = Tokenkind;

    fn step(&mut self, c: char) -> Res<Tokenkind> {
        if is_word_break(c) && !c.is_ascii_digit() {
            Res {
                consume: false,
                result: Some(Tokenkind::Name),
            }
        } else {
            Res::CONSUME
        }
    }
}

fn is_word_break(c: char) -> bool {
    c.is_ascii_digit()
        || c.is_ascii_whitespace()
        || c == '"'
        || IMMS.iter().any(|t| t.spelling().starts_with(c))
}

struct CommentLexer(Option<usize>);
impl CommentLexer {
    fn new() -> Self {
        Self(Some(0))
    }
}
impl Lexer<char> for CommentLexer {
    type Out = Tokenkind;

    fn step(&mut self, c: char) -> Res<Tokenkind> {
        let Some(count) = self.0 else {
            return Res::FAIL;
        };
        if c == '/' {
            self.0 = Some(count + 1);
            Res::CONSUME
        } else if count < 2 {
            self.0 = None;
            Res::FAIL
        } else if c == '\n' {
            Res {
                consume: true,
                result: Some(Tokenkind::Comment),
            }
        } else {
            Res::CONSUME
        }
    }
}

struct PeekRead<R>(CharRead<R>, Option<char>);
impl<R: Read> PeekRead<R> {
    fn new(r: R) -> Self {
        Self(CharRead::new(r), None)
    }
    fn next(&mut self) -> Option<char> {
        if let Some(c) = self.1.take() {
            return Some(c);
        }
        self.0.next()
    }
    fn peek(&mut self) -> Option<char> {
        if let Some(c) = self.1 {
            return Some(c);
        }
        let c = self.0.next()?;
        self.1 = Some(c);
        Some(c)
    }
}

pub struct Tokenizer<R> {
    file: files::Id,
    source: PeekRead<R>,
    offset: usize,
    interner: HashSet<&'static str>,
}
impl<R: Read> Iterator for Tokenizer<R> {
    type Item = Token<&'static str>;

    fn next(&mut self) -> Option<Self::Item> {
        self.strip_whitespace();
        let mut lexer = CommentLexer::new()
            .par(immediates())
            .par(keywords())
            .par(bools())
            .par(StringLexer::new())
            .par(NumberLexer::new())
            .par(NameLexer::new());
        let start = self.offset;
        let mut buffer = String::new();
        while let Some(c) = self.source.peek() {
            let r = lexer.step(c);
            if r.consume {
                buffer.push(c);
                let _ = self.source.next();
                self.offset += c.len_utf8();
            }
            if let Some(kind) = r.result {
                return Some(Token {
                    text: buffer.leak(),
                    loc: Provenance::Span {
                        module: self.file,
                        end: self.offset,
                        start,
                    },
                    kind,
                });
            }
        }
        let r = lexer.step('\0');
        let text = self.interner.get(buffer.as_str()).copied().unwrap_or_else(|| {
            let text = &*buffer.leak();
            self.interner.insert(text);
            text
        }) ;
        r.result.map(|kind| Token {
            text,
            loc: Provenance::Span {
                module: self.file,
                end: self.offset,
                start
            },
            kind
        })
    }
}
impl<R: Read> Tokenizer<R> {
    // pub fn new(file: files::Id, source: &'src str) -> Self {
    //     Tokenizer {
    //         file, source, offset: 0
    //     }
    // }
    pub fn new(file: files::Id, reader: R) -> Self {
        Tokenizer {
            file, source: PeekRead::new(reader), offset: 0, interner: HashSet::new()
        }
    }
    // fn cursor(&self) -> &'src str {
    //     &self.source[self.offset..]
    // }
    fn strip_whitespace(&mut self) {
        while let Some(c) = self.source.peek() {
            if c.is_ascii_whitespace() {
                let _ = self.source.next();
                self.offset += c.len_utf8();
            } else {
                return;
            }
        }
        // let len: usize = self
        //     .cursor()
        //     .chars()
        //     .take_while(|c| c.is_ascii_whitespace())
        //     .map(|c| c.len_utf8())
        //     .sum();
        // self.offset += len;
    }
}

/*
pub struct Lexer<'src> {
    module: files::Id,
    source: &'src [u8],
    offset: usize,
}

impl<'src> Lexer<'src> {
    pub fn new(module: files::Id, source: &'src str) -> Self {
        Self {
            module,
            source: source.as_bytes(),
            offset: 0,
        }
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Token<&'src str>;

    fn next(&mut self) -> Option<Self::Item> {
        self.strip_whitespace();
        if self.source.len() <= self.offset {
            return None;
        }
        None.or_else(|| self.comment())
            .or_else(|| self.immediates())
            .or_else(|| self.keywords())
            .or_else(|| self.bools())
            .or_else(|| self.string())
            .or_else(|| self.number())
            .or_else(|| self.name())
    }
}

impl<'src> Lexer<'src> {
    const fn mk_tk(t: Tokenkind) -> (&'static [u8], Tokenkind) {
        (t.spelling().as_bytes(), t)
    }
    const IMMS: &'static [(&'static [u8], Tokenkind)] = {
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
        let len = self
            .cursor()
            .iter()
            .take_while(|&&b| (b as char).is_ascii_whitespace())
            .count();
        self.offset += len;
    }

    fn comment(&mut self) -> Option<Token<&'src str>> {
        let text = self.cursor();
        let start = self.offset;
        if !text.starts_with(b"//") {
            return None;
        }
        let len = text.iter().take_while(|&&b| b != b'\n').count() + 1;
        let len = len.min(text.len());
        self.offset += len;
        Some(Token {
            kind: Tokenkind::Comment,
            text: std::str::from_utf8(&text[..len]).unwrap(),
            loc: Provenance::Span {
                module: self.module,
                start,
                end: self.offset,
            },
        })
    }

    fn with_table(
        &mut self,
        t: &[(&[u8], Tokenkind)],
        p: impl Fn(&[u8]) -> bool,
    ) -> Option<Token<&'src str>> {
        let text = self.cursor();
        let start = self.offset;
        for (s, k) in t {
            if !text.starts_with(s) {
                continue;
            }
            if !p(&text[s.len()..]) {
                continue;
            }
            self.offset += s.len();
            return Some(Token {
                loc: Provenance::Span {
                    module: self.module,
                    start,
                    end: self.offset,
                },
                text: std::str::from_utf8(&text[..s.len()]).unwrap(),
                kind: *k,
            });
        }
        None
    }

    fn immediates(&mut self) -> Option<Token<&'src str>> {
        self.with_table(Self::IMMS, |_| true)
    }

    fn keywords(&mut self) -> Option<Token<&'src str>> {
        use Tokenkind::*;
        const TABLE: &[(&[u8], Tokenkind)] = &[
            Lexer::mk_tk(Case),
            Lexer::mk_tk(Func),
            Lexer::mk_tk(Return),
            Lexer::mk_tk(Loop),
            Lexer::mk_tk(Break),
            Lexer::mk_tk(If),
            Lexer::mk_tk(Else),
            Lexer::mk_tk(Let),
            Lexer::mk_tk(Mut),
            Lexer::mk_tk(Type),
            Lexer::mk_tk(As),
            Lexer::mk_tk(Operator),
            Lexer::mk_tk(Equals),
            Lexer::mk_tk(Import),
            Lexer::mk_tk(Underscore),
        ];
        self.with_table(TABLE, |s| s.first().map_or(true, |&c| is_word_break(c)))
    }

    fn bools(&mut self) -> Option<Token<&'src str>> {
        use Tokenkind::*;
        const TABLE: &[(&[u8], Tokenkind)] = &[(b"true", Bool), (b"false", Bool)];
        self.with_table(TABLE, |s| s.first().map_or(true, |&c| is_word_break(c)))
    }

    fn string(&mut self) -> Option<Token<&'src str>> {
        let text = self.cursor();
        let start = self.offset;
        if text[0] != b'"' {
            return None;
        }
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
            text: std::str::from_utf8(&text[..len]).unwrap(),
            loc: Provenance::Span {
                module: self.module,
                start,
                end: self.offset,
            },
        })
    }

    fn number(&mut self) -> Option<Token<&'src str>> {
        let text = self.cursor();
        let start = self.offset;
        let len = text
            .iter()
            .take_while(|&&b| (b as char).is_ascii_digit())
            .count();
        self.offset += len;
        if len == 0 {
            return None;
        }
        Some(Token {
            kind: Tokenkind::Number,
            text: std::str::from_utf8(&text[..len]).unwrap(),
            loc: Provenance::Span {
                module: self.module,
                start,
                end: self.offset,
            },
        })
    }

    fn name(&mut self) -> Option<Token<&'src str>> {
        let text = self.cursor();
        let start = self.offset;
        let len = text
            .iter()
            .take_while(|&&b| !is_word_break(b) || (b as char).is_ascii_digit())
            .count();
        self.offset += len;
        if len == 0 {
            return None;
        }
        Some(Token {
            kind: Tokenkind::Name,
            text: std::str::from_utf8(&text[..len]).unwrap(),
            loc: Provenance::Span {
                module: self.module,
                start,
                end: self.offset,
            },
        })
    }
}

fn is_word_break(b: u8) -> bool {
    if (b as char).is_ascii_digit() {
        return true;
    }
    if (b as char).is_ascii_whitespace() {
        return true;
    }
    if b == b'"' {
        return true;
    }
    Lexer::IMMS.iter().any(|(s, _)| s.starts_with(&[b]))
}
*/
