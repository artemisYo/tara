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

#[derive(Debug, Clone, Copy)]
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
#[derive(Debug)]
pub enum Literal<'a> {
    Number(&'a str),
    String(&'a str),
    Name(&'a str),
}
#[derive(Debug)]
pub enum Token<'a> {
    Keyword(Keyword),
    Literal(Literal<'a>),
    Delimiter(Delimiter),
    Block(Block<'a>),
}
#[derive(Debug, Clone, Copy)]
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
pub struct TokenStream<'a>(pub Vec<(usize, Token<'a>)>);

impl Grouping {
    pub const COUNT: usize = Self::Count as usize;
}
impl Delimiter {
    pub const COUNT: usize = Self::Count as usize;
}
impl Keyword {
    pub const COUNT: usize = Self::Count as usize;
}
