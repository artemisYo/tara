pub trait Tokenizeable {
    fn tokenstream<'a>(&'a self) -> Tokenstream<'a>;
}
impl Tokenizeable for str {
    fn tokenstream<'a>(&'a self) -> Tokenstream<'a> {
        Tokenstream {
            string: self,
            index: 0,
            loc: (0, 0),
            peeked_loc: None,
            peek: None,
        }
    }
}

// this is basically the extent of the microsyntax
// whitespace is stripped before attempting to parse a token
#[derive(PartialEq, Eq)]
pub enum Token<'a> {
    Num(&'a str),   //<- '0'..'9'+
    Str(&'a str),   //<- '"' (!'"' (any | '\\' '"'))* '"'
    Char(&'a str),  //<- '\'' (!'\'' (any | '\\' '\'')) '\''
    Bool(&'a str),  //<- "true" | "false"
    OpenParen,      //<- '('
    CloseParen,     //<- ')'
    OpenBracket,    //<- '['
    CloseBracket,   //<- ']'
    Period,         //<- '.'
    DPeriod,        //<- ".."
    SemiCol,        //<- ';'
    Colon,          //<- ':'
    Star,           //<- '*'
    Arrow,          //<- "->"
    FnKey,          //<- "fn"
    ForKey,         //<- "for"
    InKey,          //<- "in"
    IfKey,          //<- "if"
    ElseKey,        //<- "else"
    LetKey,         //<- "let"
    MutKey,         //<- "mut"
    Ident(&'a str), //<- else (!whitespace any)*
}
impl<'a> Token<'a> {
    fn len(&self) -> usize {
        match self {
            Token::Num(s) => s.len(),
            Token::Str(s) => s.len(),
            Token::Char(s) => s.len(),
            Token::Bool(s) => s.len(),
            Token::OpenParen
            | Token::CloseParen
            | Token::OpenBracket
            | Token::CloseBracket
            | Token::Period
            | Token::SemiCol
            | Token::Colon
            | Token::Star => 1,
            Token::DPeriod | Token::Arrow | Token::FnKey | Token::InKey | Token::IfKey => 2,
            Token::ForKey | Token::LetKey | Token::MutKey => 3,
            Token::ElseKey => 4,
            Token::Ident(s) => s.len(),
        }
    }
    pub fn parse(string: &str) -> Self {
        match string.chars().nth(0).unwrap() {
            c if c.is_ascii_digit() => tokenize_num(string),
            c if c == '"' => tokenize_strlit(string),
            c if c == '\'' => tokenize_char(string),
            _ if string.starts_with("true") => Token::Bool(&string[..4]),
            _ if string.starts_with("false") => Token::Bool(&string[..5]),
            _ if string.starts_with("(") => Token::OpenParen,
            _ if string.starts_with(")") => Token::CloseParen,
            _ if string.starts_with("[") => Token::OpenBracket,
            _ if string.starts_with("]") => Token::CloseBracket,
            _ if string.starts_with("..") => Token::DPeriod,
            _ if string.starts_with(".") => Token::Period,
            _ if string.starts_with(";") => Token::SemiCol,
            _ if string.starts_with(":") => Token::Colon,
            _ if string.starts_with("*") => Token::Star,
            _ if string.starts_with("->") => Token::Arrow,
            _ if string.starts_with("fn") => Token::FnKey,
            _ if string.starts_with("for") => Token::ForKey,
            _ if string.starts_with("in") => Token::InKey,
            _ if string.starts_with("if") => Token::IfKey,
            _ if string.starts_with("else") => Token::ElseKey,
            _ if string.starts_with("let") => Token::LetKey,
            _ if string.starts_with("mut") => Token::MutKey,
            _ => Token::Ident(&string[..string.find(char::is_whitespace).unwrap_or(string.len())]),
        }
    }
}

pub struct Tokenstream<'a> {
    string: &'a str,
    index: usize,
    loc: (usize, usize),
    peeked_loc: Option<(usize, usize)>,
    peek: Option<Option<Token<'a>>>,
}
impl<'a> Tokenstream<'a> {
    fn skip_whitespace(&mut self) {
        self.advance_bytes(
            self.get()
                .chars()
                .take_while(|c| c.is_whitespace())
                .map(|c| c.len_utf8())
                .sum(),
        );
    }
    fn get(&self) -> &str {
        &self.string[self.index..]
    }
    fn advance(&mut self, x: usize) {
        let bytes = self
            .get()
            .chars()
            .take(x)
            .map(|c| c.len_utf8())
            .sum::<usize>();
        self.loc.0 += self.get()[..bytes].chars().filter(|c| c == &'\n').count();
        match self.get()[..bytes].rfind('\n') {
            Some(n) => self.loc.1 = bytes - n,
            None => self.loc.1 += bytes,
        }
        self.index += bytes;
    }
    fn advance_bytes(&mut self, x: usize) {
        self.loc.0 += self.get()[..x].chars().filter(|c| c == &'\n').count();
        match self.get()[..x].rfind('\n') {
            Some(n) => self.loc.1 = x - n,
            None => self.loc.1 += x,
        }
        self.index += x;
    }
}
impl<'a> Tokenstream<'a> {
    pub(super) fn get_loc(&self) -> (usize, usize) {
        match self.peeked_loc {
            None => self.loc,
            Some(l) => l,
        }
    }
    pub fn peek(&mut self) -> Option<&Token<'a>> {
        match self.peek {
            None => {
                self.peeked_loc = Some(self.loc);
                self.peek = Some(self.next());
                self.peek()
            }
            Some(t) => t.as_ref(),
        }
    }
}
impl<'a> std::iter::Iterator for Tokenstream<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.peek.take() {
            Some(t) => {
                self.peeked_loc = None;
                t
            }
            None => {
                self.skip_whitespace();
                let string = self.get();
                if string.is_empty() {
                    return None;
                }
                let token = Token::parse(string);
                self.advance_bytes(token.len());
                Some(token)
            }
        }
    }
}

fn tokenize_num(string: &str) -> Token {
    Token::Num(
        &string[..string
            .chars()
            .take_while(|c| c.is_ascii_digit())
            .map(|c| c.len_utf8())
            .sum()],
    )
}
fn tokenize_strlit(string: &str) -> Token {
    for i in 1..string.len() {
        if (&string[i..i + 1] == "\n") | (&string[i..i + 1] == "\"" && &string[i - 1..i] != "\\") {
            return Token::Str(&string[0..i + 1]);
        }
    }
    return Token::Str(string);
}
fn tokenize_char(string: &str) -> Token {
    // this implementation, so as to avoid returning None before the
    // iterator is empty accepts the following patterns:
    //  1. ' ε
    //  2. ''
    //  3. 'c
    //  4. 'c'
    //  5. '\c
    //  6. '\c'
    match string.chars().nth(1) {
        Some('\'') => {
            return Token::Char(&string[..string.chars().take(2).map(|c| c.len_utf8()).sum()])
        }
        Some('\\') => {
            let n = if string.chars().nth(3) == Some('\'') {
                4
            } else {
                3
            };
            return Token::Char(&string[..string.chars().take(n).map(|c| c.len_utf8()).sum()]);
        }
        Some(_) => {
            let n = if string.chars().nth(2) == Some('\'') {
                3
            } else {
                2
            };
            return Token::Char(&string[..string.chars().take(n).map(|c| c.len_utf8()).sum()]);
        }
        None => return Token::Char(string),
    }
}
