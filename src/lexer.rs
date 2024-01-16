#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token<'a> {
    // Keywords
    If,
    Else,
    While,
    Then,
    Let,
    // Operators
	Semicolon,
    Plus,
    Minus,
    Star,
    Slash,
    Equals,
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    // Literals
    Number(isize),
    Name(&'a str)
}

#[derive(Debug, Clone, Copy)]
pub struct TokenIter<'a>(&'a [Token<'a>]);
impl<'a> TokenIter<'a> {
    pub fn new(input: &'a [Token<'a>]) -> Self {
        Self(input)
    }
    pub fn consume(&mut self, t: Token<'_>) -> Option<Token<'a>> {
        let out = *self.0.get(0)?;
        if out == t {
            self.next();
            return Some(out);
        }
        None
    }
    pub fn find_map<T, F>(&mut self, f: F) -> Option<T>
    where F: FnOnce(Token<'_>) -> Option<T> {
        let t = *self.0.get(0)?;
        if let Some(o) = f(t) {
            self.next();
            return Some(o);
        }
        None
    }
    pub fn expect_done(&self) {
		if self.0.len() > 0 {
			panic!("Expected iterator to be done!\n{:?}", self.0);
		}
    }
}
impl<'a> std::iter::Iterator for TokenIter<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let out = self.0.get(0)?;
        self.0 = &self.0[1..];
        Some(*out)
    }
}

pub fn tokenize<'a>(mut input: &'a str) -> Result<Vec<Token<'a>>, &'a str> {
    let mut token_stream = vec![];
    input = input.trim_start();
    while input.len() > 0 {
        const TOKS: &[Tokenizer] = &[
            lex_operator,
            lex_number,
            lex_keyword,
            lex_name,
        ];
        let (rest, t) = TOKS.iter()
            .map(|f| f(input))
            .find(Option::is_some)
            .flatten()
            .ok_or(input)?;
        token_stream.push(t);
        input = rest.trim_start();
    }
    Ok(token_stream)
}

type Tokenizer = fn(&str) -> Option<(&str, Token)>;

const OPERATORS: &[(&str, Token)] = {
    use Token::*;
    &[("+", Plus)
    , ("*", Star)
    , ("-", Minus)
    , ("/", Slash)
    , ("=", Equals)
    , (";", Semicolon)
    , ("(", OpenParen)
    , (")", CloseParen)
    , ("{", OpenCurly)
    , ("}", CloseCurly)
    ]
};

fn starts_operator(input: char) -> bool {
    OPERATORS.iter().any(|(o, _)| o.starts_with(input))
}

fn lex_operator(input: &str) -> Option<(&str, Token)> {
    OPERATORS.iter()
        .find(|(o, _)| input.starts_with(o))
        .map(|(o, t)| (&input[o.len()..], *t))
}

fn lex_number(input: &str) -> Option<(&str, Token)> {
    let len = input.chars()
        .take_while(char::is_ascii_digit)
        .map(char::len_utf8)
        .sum::<usize>();
    if len == 0 { return None; }
    let (num, rest) = input.split_at(len);
    let num = num.parse::<isize>().ok()?;
    Some((rest, Token::Number(num)))
}

fn lex_name(input: &str) -> Option<(&str, Token)> {
    let len = input.chars()
        .take_while(|c| !c.is_whitespace() && !starts_operator(*c))
        .map(char::len_utf8)
        .sum::<usize>();
    if len == 0 { return None; }
    let (name, rest) = input.split_at(len);
    Some((rest, Token::Name(name)))
}

fn lex_keyword(input: &str) -> Option<(&str, Token)> {
    fn starts_delimited(string: &str) -> bool {
		string.chars().nth(0).map(|c| c.is_whitespace() || starts_operator(c)).unwrap_or(true)
    }
	const KEYS: &[(&str, Token)] = &[
        ("if", Token::If),
		("let", Token::Let),
        ("then", Token::Then),
        ("else", Token::Else),
        ("while", Token::While),
    ];
	KEYS.iter()
    	.find(|(k, _)| input.starts_with(k) && starts_delimited(&input[k.len()..]))
    	.map(|(k, t)| (&input[k.len()..], *t))
}
