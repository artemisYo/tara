// this is basically the extent of the microsyntax
// whitespace is stripped before attempting to parse a token
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Token {
    // Comments are skipped:
    // Comment        <- "//" (!'\n' any)*
    Num(String),   //<- '0'..'9'+
    Str(String),   //<- '"' (!'"' (any | '\\' '"'))* '"'
    Char(String),  //<- '\'' (!'\'' (any | '\\' '\'')) '\''
    Bool(String),  //<- "true" | "false"
    Plus,          //<- '+'
    OpenParen,     //<- '('
    CloseParen,    //<- ')'
    OpenBracket,   //<- '['
    CloseBracket,  //<- ']'
    Accessor,      //<- '.' !whitespace
    Period,        //<- '.' &whitespace
    DPeriod,       //<- ".."
    Comma,         //<- ","
    SemiCol,       //<- ';'
    Colon,         //<- ':'
    Star,          //<- '*'
    Equals,        //<- '='
    Arrow,         //<- "->"
    FnKey,         //<- "fn"
    ForKey,        //<- "for"
    InKey,         //<- "in"
    OnKey,         //<- "on"
    IfKey,         //<- "if"
    ElseKey,       //<- "else"
    LetKey,        //<- "let"
    MutKey,        //<- "mut"
    MatchKey,      //<- "match"
    CaseKey,       //<- "case"
    TypeKey,       //<- "type"
    ProtoKey,      //<- "proto"
    ImplKey,       //<- "impl"
    Ident(String), //<- else (!whitespace any)*
}
impl Token {
    fn len(&self) -> usize {
        match self {
            Token::Num(s) => s.len(),
            Token::Str(s) => s.len(),
            Token::Char(s) => s.len(),
            Token::Bool(s) => s.len(),
            Token::OpenParen
            | Token::Accessor
            | Token::CloseParen
            | Token::OpenBracket
            | Token::CloseBracket
            | Token::Period
            | Token::SemiCol
            | Token::Colon
            | Token::Star
            | Token::Comma
            | Token::Plus
            | Token::Equals => 1,
            Token::OnKey
            | Token::DPeriod
            | Token::Arrow
            | Token::FnKey
            | Token::InKey
            | Token::IfKey => 2,
            Token::ForKey | Token::LetKey | Token::MutKey => 3,
            Token::ImplKey | Token::CaseKey | Token::TypeKey | Token::ElseKey => 4,
            Token::ProtoKey | Token::MatchKey => 5,
            Token::Ident(s) => s.len(),
        }
    }
    pub fn parse(string: &str) -> Self {
        match string.chars().nth(0).unwrap() {
            c if c.is_ascii_digit() => tokenize_num(string),
            c if c == '"' => tokenize_strlit(string),
            c if c == '\'' => tokenize_char(string),
            _ if string.starts_with("true") => Token::Bool(string[..4].to_owned()),
            _ if string.starts_with("false") => Token::Bool(string[..5].to_owned()),
            _ if string.starts_with("..") => Token::DPeriod,
            '+' => Token::Plus,
            '(' => Token::OpenParen,
            ')' => Token::CloseParen,
            '[' => Token::OpenBracket,
            ']' => Token::CloseBracket,
            '.' => {
                if string.chars().nth(1).is_some_and(|c| c.is_whitespace()) {
                    Token::Period
                } else {
                    Token::Accessor
                }
            }
            ',' => Token::Comma,
            ';' => Token::SemiCol,
            ':' => Token::Colon,
            '*' => Token::Star,
            '=' => Token::Equals,
            _ if string.starts_with("->") => Token::Arrow,
            _ if string.starts_with("fn") => Token::FnKey,
            _ if string.starts_with("on") => Token::OnKey,
            _ if string.starts_with("for") => Token::ForKey,
            _ if string.starts_with("in") => Token::InKey,
            _ if string.starts_with("if") => Token::IfKey,
            _ if string.starts_with("else") => Token::ElseKey,
            _ if string.starts_with("case") => Token::CaseKey,
            _ if string.starts_with("type") => Token::TypeKey,
            _ if string.starts_with("proto") => Token::ProtoKey,
            _ if string.starts_with("match") => Token::MatchKey,
            _ if string.starts_with("impl") => Token::ImplKey,
            _ if string.starts_with("let") => Token::LetKey,
            _ if string.starts_with("mut") => Token::MutKey,
            // change this to not recognize strings like "Vec:push"
            // as one Ident
            _ => Token::Ident(
                string[..string
                    .find(|c: char| {
                        c.is_whitespace()
                            || [
                                '.', ',', ':', ';', '*', '-', '"', '\'', '+', '[', ']', '(', ')',
                                '=',
                            ]
                            .contains(&c)
                    })
                    .unwrap_or(string.len())]
                    .to_owned(),
            ),
        }
    }
    pub fn is_ident(&self) -> bool {
        match self {
            Self::Ident(_) => true,
            _ => false,
        }
    }
    pub fn is_num(&self) -> bool {
        match self {
            Self::Num(_) => true,
            _ => false,
        }
    }
    pub fn as_usize(&self) -> usize {
        match self {
            Self::Num(x) => x.parse().unwrap(),
            _ => panic!("tried to make a non-num token usize"),
        }
    }
}

#[derive(Debug)]
pub struct Tokenstream(Vec<Token>);
impl Tokenstream {
    pub fn new(mut input: &str) -> Self {
        let mut acc = vec![];
        while !input.is_empty() {
            input = input.trim_start();
            if input.starts_with("//") {
                input = &input[input.find('\n').unwrap_or(input.len())..];
            } else {
                if input.is_empty() {
                    break;
                }
                let t = Token::parse(input);
                input = &input[t.len()..];
                acc.push(t);
            }
        }
        Self(acc)
    }
    pub fn stack(&self) -> Tokenstack {
        Tokenstack(&self.0, 0)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Tokenstack<'a>(&'a [Token], usize);
impl<'a> Tokenstack<'a> {
    pub fn lookahead(&self, x: usize) -> &Token {
        &self.0[self.1 + x]
    }
    pub fn peek(&self) -> &Token {
        &self.0[self.1]
    }
    pub fn pop(&mut self) -> Token {
        if self.0.len() <= self.1 {
            panic!("Tried to pop an empty stack!");
        }
        self.1 += 1;
        self.0[self.1 - 1].clone()
    }
    pub fn unpop(&mut self) {
        if self.1 == 0 {
            panic!("Tried to unpop a full stack!");
        }
        self.1 -= 1;
    }
    pub fn is_empty(&self) -> bool {
        self.0.len() == self.1
    }
}

fn tokenize_num(string: &str) -> Token {
    Token::Num(
        string[..string
            .chars()
            .take_while(|c| c.is_ascii_digit())
            .map(|c| c.len_utf8())
            .sum()]
            .to_owned(),
    )
}
fn tokenize_strlit(string: &str) -> Token {
    for i in 1..string.len() {
        if (&string[i..i + 1] == "\n") | (&string[i..i + 1] == "\"" && &string[i - 1..i] != "\\") {
            return Token::Str(string[0..i + 1].to_owned());
        }
    }
    return Token::Str(string.to_owned());
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
            return Token::Char(
                string[..string.chars().take(2).map(|c| c.len_utf8()).sum()].to_owned(),
            )
        }
        Some('\\') => {
            let n = if string.chars().nth(3) == Some('\'') {
                4
            } else {
                3
            };
            return Token::Char(
                string[..string.chars().take(n).map(|c| c.len_utf8()).sum()].to_owned(),
            );
        }
        Some(_) => {
            let n = if string.chars().nth(2) == Some('\'') {
                3
            } else {
                2
            };
            return Token::Char(
                string[..string.chars().take(n).map(|c| c.len_utf8()).sum()].to_owned(),
            );
        }
        None => return Token::Char(string.to_owned()),
    }
}
