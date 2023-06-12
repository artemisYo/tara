trait StartsWithStringAndSpace {
    fn starts_with_n_stuff(&self, _: &'static str, _: &[char]) -> bool;
}
impl StartsWithStringAndSpace for str {
    fn starts_with_n_stuff(&self, p: &'static str, c: &[char]) -> bool {
        self.starts_with(p)
            && (self.len() == p.len()
                || self.chars().nth(p.len()).unwrap().is_whitespace()
                || c.contains(&self.chars().nth(p.len()).unwrap()))
    }
}

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
    Arrow,         //<- "->"
    Plus,          //<- '+'
    Minus,         //<- '-'
    OpenParen,     //<- '('
    CloseParen,    //<- ')'
    OpenBracket,   //<- '['
    CloseBracket,  //<- ']'
    OpenCurly,     //<- "{"
    CloseCurly,    //<- "}"
    OpenKeter,     //<- "<"
    CloseKeter,    //<- ">"
    Accessor,      //<- '.' !whitespace
    Period,        //<- '.' &whitespace
    DPeriod,       //<- ".."
    Comma,         //<- ","
    SemiCol,       //<- ';'
    Colon,         //<- ':'
    Star,          //<- '*'
    Ampersand,     //<- '&'
    Equals,        //<- '='
    Slash,         //<- '/'
    FnKey,         //<- "fn" &whitespace
    ForKey,        //<- "for" &whitespace
    InKey,         //<- "in" &whitespace
    OnKey,         //<- "on" &whitespace
    IfKey,         //<- "if" &whitespace
    ElseKey,       //<- "else" &whitespace
    LetKey,        //<- "let" &whitespace
    MutKey,        //<- "mut" &whitespace
    MatchKey,      //<- "match" &whitespace
    CaseKey,       //<- "case" &whitespace
    TypeKey,       //<- "type" &whitespace
    ProtoKey,      //<- "proto" &whitespace
    ReadableKey,   //<- "readable" &whitespace
    WriteableKey,  //<- "writeable" &whitespace
    ImplKey,       //<- "impl" &whitespace
    DoKey,         //<- "do" &whitespace
    EndKey,        //<- "end" &whitespace
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
            | Token::OpenCurly
            | Token::CloseCurly
            | Token::OpenKeter
            | Token::CloseKeter
            | Token::Period
            | Token::SemiCol
            | Token::Colon
            | Token::Star
            | Token::Comma
            | Token::Plus
            | Token::Minus
            | Token::Equals
            | Token::Ampersand
            | Token::Slash => 1,
            Token::OnKey
            | Token::DPeriod
            | Token::Arrow
            | Token::FnKey
            | Token::InKey
            | Token::IfKey
            | Token::DoKey => 2,
            Token::ForKey | Token::LetKey | Token::MutKey | Token::EndKey => 3,
            Token::ImplKey | Token::CaseKey | Token::TypeKey | Token::ElseKey => 4,
            Token::ProtoKey | Token::MatchKey => 5,
            Token::ReadableKey => 8,
            Token::WriteableKey => 9,
            Token::Ident(s) => s.len(),
        }
    }
    pub fn parse(string: &str) -> Option<Self> {
        match string.chars().nth(0).unwrap() {
            c if c.is_ascii_digit() => Some(tokenize_num(string)),
            c if c == '"' => Some(tokenize_strlit(string)),
            c if c == '\'' => Some(tokenize_char(string)),
            _ if string.starts_with("true") => Some(Token::Bool(string[..4].to_owned())),
            _ if string.starts_with("false") => Some(Token::Bool(string[..5].to_owned())),
            _ if string.starts_with("..") => Some(Token::DPeriod),
            '+' => Some(Token::Plus),
            '(' => Some(Token::OpenParen),
            ')' => Some(Token::CloseParen),
            '[' => Some(Token::OpenBracket),
            ']' => Some(Token::CloseBracket),
            '{' => Some(Token::OpenCurly),
            '}' => Some(Token::CloseCurly),
            '.' => {
                if string.chars().nth(1).is_some_and(|c| c.is_whitespace()) {
                    Some(Token::Period)
                } else {
                    Some(Token::Accessor)
                }
            }
            ',' => Some(Token::Comma),
            ';' => Some(Token::SemiCol),
            ':' => Some(Token::Colon),
            '*' => Some(Token::Star),
            '&' => Some(Token::Ampersand),
            '=' => Some(Token::Equals),
            '/' => Some(Token::Slash),
            '<' => Some(Token::OpenKeter),
            '>' => Some(Token::CloseKeter),
            _ if string.starts_with_n_stuff("->", &[]) => Some(Token::Arrow),
            '-' => Some(Token::Minus),
            _ if string.starts_with_n_stuff("fn", &['(']) => Some(Token::FnKey),
            _ if string.starts_with_n_stuff("on", &[]) => Some(Token::OnKey),
            _ if string.starts_with_n_stuff("do", &[]) => Some(Token::DoKey),
            _ if string.starts_with_n_stuff("end", &[]) => Some(Token::EndKey),
            _ if string.starts_with_n_stuff("for", &[]) => Some(Token::ForKey),
            _ if string.starts_with_n_stuff("in", &[]) => Some(Token::InKey),
            _ if string.starts_with_n_stuff("if", &[]) => Some(Token::IfKey),
            _ if string.starts_with_n_stuff("else", &[]) => Some(Token::ElseKey),
            _ if string.starts_with_n_stuff("case", &[]) => Some(Token::CaseKey),
            _ if string.starts_with_n_stuff("type", &[]) => Some(Token::TypeKey),
            _ if string.starts_with_n_stuff("proto", &[]) => Some(Token::ProtoKey),
            _ if string.starts_with_n_stuff("match", &[]) => Some(Token::MatchKey),
            _ if string.starts_with_n_stuff("impl", &[]) => Some(Token::ImplKey),
            _ if string.starts_with_n_stuff("let", &[]) => Some(Token::LetKey),
            _ if string.starts_with_n_stuff("mut", &[]) => Some(Token::MutKey),
            _ if string.starts_with_n_stuff("readable", &[]) => Some(Token::ReadableKey),
            _ if string.starts_with_n_stuff("writeable", &[]) => Some(Token::WriteableKey),
            _ => {
                let x = string
                    .find(|c: char| {
                        c.is_whitespace()
                            || [
                                '.', ',', ':', ';', '*', '"', '\'', '+', '[', ']', '(', ')', '=',
                            ]
                            .contains(&c)
                    })
                    .unwrap_or(string.len());
                if x == 0 {
                    return None;
                }
                Some(Token::Ident(string[..x].to_owned()))
            }
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
                match Token::parse(input) {
                    Some(t) => {
                        input = &input[t.len()..];
                        acc.push(t);
                    }
                    None => {
                        input = &input[1..];
                    }
                }
            }
        }
        Self(acc)
    }
    pub fn stack(&self) -> Tokenstack {
        Tokenstack(&self.0, 0)
    }
}

pub trait TokenPredicate {
    fn eval(self, _: &Token) -> bool;
}
impl TokenPredicate for Token {
    fn eval(self, t: &Token) -> bool {
        &self == t
    }
}
impl<T> TokenPredicate for T
where
    T: FnOnce(&Token) -> bool,
{
    fn eval(self, t: &Token) -> bool {
        self(t)
    }
}

#[derive(Clone, Copy)]
pub struct Tokenstack<'a>(&'a [Token], usize);
impl<'a> Tokenstack<'a> {
    pub fn peek(&self) -> &Token {
        &self.0[self.1]
    }
    pub fn pop(&mut self) -> Token {
        if self.is_empty() {
            panic!("Tried to pop an empty stack!");
        }
        self.1 += 1;
        self.0[self.1 - 1].clone()
    }
    pub fn pop_if(&mut self, predicate: impl TokenPredicate) -> Option<Token> {
        if self.next_is(predicate) {
            Some(self.pop())
        } else {
            None
        }
    }
    pub fn next_is(&self, predicate: impl TokenPredicate) -> bool {
        if self.is_empty() {
            return false;
        }
        if predicate.eval(&self.0[self.1]) {
            true
        } else {
            false
        }
    }
    pub fn is_empty(&self) -> bool {
        self.0.len() <= self.1
    }
}
impl std::fmt::Debug for Tokenstack<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", &self.0[self.1..])
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
