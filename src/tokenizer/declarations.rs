pub const SKIP: &[char] = &[' ', '\t', '\n'];

pub const KEYWORDS: [Keyword; Keyword::COUNT] = [Keyword::Fn, Keyword::Return];

pub const GROUPINGS: [Grouping; Grouping::COUNT] =
    [Grouping::Braces, Grouping::Paren, Grouping::Bracket];

pub const DELIMITERS: [Delimiter; Delimiter::COUNT] = [
    Delimiter::Semicolon,
    Delimiter::Colon,
    Delimiter::Dot,
    Delimiter::Comma,
    Delimiter::LessThan,
    Delimiter::MoreThan,
    Delimiter::Equal,
    Delimiter::Plus,
    Delimiter::Minus,
    Delimiter::Asterisk,
    Delimiter::Slash,
    Delimiter::Backslash,
    Delimiter::Pipe,
    Delimiter::Ampersand,
    Delimiter::Caret,
    Delimiter::ExclMark,
    Delimiter::QstMark,
];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Fn,
    Return,
    Count,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Grouping {
    Braces,
    Paren,
    Bracket,
    Count,
}
#[derive(Debug)]
pub struct Block<'a> {
    pub grouping: Grouping,
    pub stream: TokenStream<'a>,
}
impl PartialEq for Block<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.grouping == other.grouping
    }
}
impl Eq for Block<'_> {}
#[derive(Debug)]
pub enum Literal<'a> {
    Number(&'a str),
    String(&'a str),
    Name(&'a str),
}
impl PartialEq for Literal<'_> {
    fn eq(&self, other: Self) -> bool {
        match self {
            Self::Number(_) => {
                if let Self::Number(_) = other {
                    true
                } else {
                    false
                }
            }
            Self::String(_) => {
                if let Self::String(_) = other {
                    true
                } else {
                    false
                }
            }
            Self::Name(_) => {
                if let Self::Name(_) = other {
                    true
                } else {
                    false
                }
            }
        }
    }
}
impl Eq for Literal<'_> {}
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token<'a> {
    Keyword(Keyword),
    Literal(Literal<'a>),
    Delimiter(Delimiter),
    Block(Block<'a>),
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Delimiter {
    Semicolon,
    Colon,
    Dot,
    Comma,
    LessThan,
    MoreThan,
    Equal,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Backslash,
    Pipe,
    Ampersand,
    Caret,
    ExclMark,
    QstMark,
    Count,
}
#[derive(Debug)]
pub struct TokenStream<'a> {
    pub stream: Vec<(usize, Token<'a>)>,
    index: usize,
    offset: usize,
}
impl TokenStream<'_> {
    pub fn from<'a>(stream: Vec<(usize, Token<'a>)>, offset: usize) -> TokenStream<'a> {
        return TokenStream {
            stream,
            index: 0,
            offset,
        };
    }
    pub fn new(offset: usize) -> Self {
        return Self {
            stream: Vec::new(),
            index: 0,
            offset,
        };
    }
    pub fn peek<'a>(&'a self) -> &'a Token<'_> {
        return unsafe {
            &self
                .stream
                .get(self.index + self.offset)
                .unwrap_unchecked()
                .1
        };
    }
    pub fn shift(&mut self) {
        if self.stream.len() > self.index {
            self.index += 1;
        }
    }
    pub fn pos(&self) -> usize {
        return unsafe {
            self.stream
                .get(self.index + self.offset)
                .unwrap_unchecked()
                .0
                + self.offset
        };
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    Keyword,
    Literal,
    Delimiter,
    Block,
}
impl PartialEq<Token<'_>> for TokenType {
    fn eq(&self, other: &Token<'_>) -> bool {
        match self {
            Self::Keyword => {
                if let Token::Keyword(_) = other {
                    true
                } else {
                    false
                }
            }
            Self::Literal => {
                if let Token::Literal(_) = other {
                    true
                } else {
                    false
                }
            }
            Self::Delimiter => {
                if let Token::Delimiter(_) = other {
                    false
                } else {
                    true
                }
            }
            Self::Block => {
                if let Token::Block(_) = other {
                    false
                } else {
                    true
                }
            }
        }
    }
}
impl PartialEq<TokenType> for Token<'_> {
    fn eq(&self, other: &TokenType) -> bool {
        match self {
            Self::Keyword(_) => {
                if other == &TokenType::Keyword {
                    true
                } else {
                    false
                }
            }
            Self::Literal(_) => {
                if other == &TokenType::Literal {
                    true
                } else {
                    false
                }
            }
            Self::Delimiter(_) => {
                if other == &TokenType::Delimiter {
                    true
                } else {
                    false
                }
            }
            Self::Block(_) => {
                if other == &TokenType::Block {
                    true
                } else {
                    false
                }
            }
        }
    }
}
impl Grouping {
    pub const COUNT: usize = Self::Count as usize;
}
impl Delimiter {
    pub const COUNT: usize = Self::Count as usize;
}
impl Keyword {
    pub const COUNT: usize = Self::Count as usize;
}

impl Token<'_> {
    pub fn expect_type(&self, rhs: TokenType) -> bool {
        self == &rhs
    }
    pub fn expect(&self, rhs: Token<'_>) -> bool {
        self == &rhs
    }
}
