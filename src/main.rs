use std::ops::Deref;

type Result<'a> = std::result::Result<(AST, Str<'a>), ParseError>;

enum ParseError {
    EndOfString,
}

enum AST {
    Prog(Vec<Self>),
}

#[derive(Clone, Copy)]
struct Str<'a> {
    string: &'a str,
    index: usize,
    pos: (usize, usize),
}
impl<'a> Str<'a> {
    fn advance(&mut self, x: usize) {
        self.pos.0 += self.string[self.index..self.index + x]
            .chars()
            .map(|c| if c == '\n' { 1 } else { 0 })
            .sum::<usize>();
        if let Some(i) = self.string[self.index..self.index + x].rfind('\n') {
            self.pos.1 = x - i;
        } else {
            self.pos.1 += x;
        }
        self.index += x;
    }
    fn check(&mut self, start: &str) -> bool {
        if self.starts_with(start) {
            self.advance(start.len());
            true
        } else {
            false
        }
    }
}

impl<'a> Deref for Str<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.string[self.index..]
    }
}

// programm   <- function*
fn parse_programm(input: Str) -> Result {
    let mut acc = vec![];
    let mut head = input;
    while let Ok((a, s)) = parse_function(head) {
        head = s;
        acc.push(a);
    }
    return Ok((AST::Prog(acc), head));
}

// function   <- "fn" name %todo from here% "(" paramList? ")" typeSig? "->" exprList? ("." | ";")
fn parse_function(mut input: Str) -> Result {
    let name;
    if !input.check("fn") {
        todo!();
        //return
    }
    match parse_name(input) {
        Ok((a, s)) => {
            input = s;
            name = a;
        }
        Err(e) => {
            todo!()
            //return
        }
    }
}

fn main() {
    println!("Hello, world!");
}
