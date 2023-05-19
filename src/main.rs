use std::ops::Deref;

type Result<'a> = std::result::Result<(AST<'a>, Str<'a>), ParseError>;

#[derive(Debug)]
enum ParseError {
    EndOfString,
    TodoError,
    Expected(&'static str),
    Multiple(Vec<Self>),
    FuncNameError(Box<Self>),
    ParamListEmpty(Box<Self>),
    ParamNameError(Box<Self>),
    ParamTypeSigError(Box<Self>),
    ExprListEmpty(Box<Self>),
    NormExprError([Box<Self>; 4]),
    LiteralError([Box<Self>; 4]),
    BlockExprError([Box<Self>; 3]),
    ForStmtIdentError(Box<Self>),
    ForStmtSourceError(Box<Self>),
    NameNotAlpha,
    NumNotDigit,
    CharNotAccepted,
    BollNotBool,
}

#[derive(Debug)]
enum AST<'a> {
    Prog(Vec<Self>),
    Func {
        name: Box<Self>,
        parameters: Option<Box<Self>>,
        ret_type: Option<Box<Self>>,
        body: Option<Box<Self>>,
        returns: bool,
    },
    ForStmt {
        ident: Box<Self>,
        source: Box<Self>,
        body: Option<Box<Self>>,
        returns: bool,
    },
    ParamList {
        body: Option<Vec<Self>>,
        last: Box<Self>,
    },
    Parameter {
        name: Box<Self>,
        type_sig: Box<Self>,
    },
    ExprList {
        body: Option<Vec<Self>>,
        last: Box<Self>,
    },
    Name(&'a str),
    Number(&'a str),
    String(&'a str),
    Char(&'a str),
    Bool(&'a str),
}

#[derive(Clone, Copy, Debug)]
struct Str<'a> {
    string: &'a str,
    index: usize,
    pos: (usize, usize),
}
impl<'a> Str<'a> {
    fn new(string: &'a str) -> Self {
        Self {
            string,
            index: 0,
            pos: (0, 0),
        }
    }
    fn advance(&mut self, x: usize) {
        if self.index + x > self.string.len() {
            panic!("Str advanced to out of bounds!");
        }
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
    fn trim_start(&mut self) {
        self.advance(self.chars().take_while(|c| c.is_whitespace()).count());
    }
    fn get_until(&self, i: usize) -> Option<&str> {
        self.deref().get(..i)
    }
    fn check(&mut self, start: &str) -> bool {
        if self.starts_with(start) {
            self.advance(start.len());
            self.trim_start();
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
    loop {
        match parse_function(head) {
            Ok((a, s)) => {
                head = s;
                acc.push(a);
            }
            Err(e) => {
                println!("{:?}", e);
                break;
            }
        }
    }
    return Ok((AST::Prog(acc), head));
}

// function   <- "fn" name "(" paramList? ")" typeSig? "->" exprList? ("." | ";")
fn parse_function(mut input: Str) -> Result {
    let name;
    let mut parameters = None;
    let mut ret_type = None;
    let mut body = None;
    let mut does_return = true;
    if !input.check("fn") {
        return Err(ParseError::Expected("fn"));
    }
    match parse_name(input) {
        Ok((a, s)) => {
            input = s;
            name = Box::new(a);
        }
        Err(e) => {
            return Err(ParseError::FuncNameError(Box::new(e)));
        }
    }
    if !input.check("(") {
        return Err(ParseError::Expected("("));
    }
    if let Ok((a, s)) = parse_param_list(input) {
        input = s;
        parameters = Some(Box::new(a));
    }
    if !input.check(")") {
        return Err(ParseError::Expected(")"));
    }
    if let Ok((a, s)) = parse_type_sig(input) {
        input = s;
        ret_type = Some(Box::new(a));
    }
    if !input.check("->") {
        return Err(ParseError::Expected("->"));
    }
    if let Ok((a, s)) = parse_expr_list(input) {
        input = s;
        body = Some(Box::new(a));
    }
    if !input.check(".") {
        does_return = false;
        if !input.check(";") {
            return Err(ParseError::Multiple(vec![
                ParseError::Expected("."),
                ParseError::Expected(";"),
            ]));
        }
    }
    Ok((
        AST::Func {
            name,
            parameters,
            ret_type,
            body,
            returns: does_return,
        },
        input,
    ))
}

// name       <- alpha+
fn parse_name(mut input: Str) -> Result {
    let i = input
        .chars()
        .take_while(|c| c.is_ascii_alphabetic())
        .count();
    if i == 0 {
        return Err(ParseError::NameNotAlpha);
    }
    // for some god forsaken reason I cannot borrow the underlying
    // string unless its done like this
    let name = &input.string[input.index..input.index + i];
    input.advance(i);
    input.trim_start();
    Ok((AST::Name(name), input))
}

// paramList  <- (parameter ",")* parameter ","?
fn parse_param_list(mut input: Str) -> Result {
    let mut params = vec![];
    let last;
    while let Ok((a, s)) = match parse_parameter(input) {
        Ok((tree, mut head)) => {
            if !head.check(",") {
                Err(())
            } else {
                Ok((tree, head))
            }
        }
        Err(_) => Err(()),
    } {
        input = s;
        params.push(a);
    }
    match parse_parameter(input) {
        Ok((a, s)) => {
            input = s;
            last = Box::new(a);
        }
        Err(e) => {
            return Err(ParseError::ParamListEmpty(Box::new(e)));
        }
    }
    input.check(",");
    let body = if params.is_empty() {
        None
    } else {
        Some(params)
    };
    Ok((AST::ParamList { body, last }, input))
}

// parameter  <- name typeSig
fn parse_parameter(mut input: Str) -> Result {
    let name;
    let type_sig;
    match parse_name(input) {
        Ok((a, s)) => {
            name = Box::new(a);
            input = s;
        }
        Err(e) => {
            return Err(ParseError::ParamNameError(Box::new(e)));
        }
    }
    match parse_type_sig(input) {
        Ok((a, s)) => {
            type_sig = Box::new(a);
            input = s;
        }
        Err(e) => {
            return Err(ParseError::ParamTypeSigError(Box::new(e)));
        }
    }
    Ok((AST::Parameter { name, type_sig }, input))
}

// typeSig    <- ":" name
// TODO: this thang doesn't parse more complex types such as
// *str or [int] or whatever
fn parse_type_sig(mut input: Str) -> Result {
    if !input.check(":") {
        return Err(ParseError::Expected(":"));
    }
    parse_name(input)
}

// exprList   <- ((normExpr ",") | (blockExpr ","?))* (normExpr | blockExpr) ","?
fn parse_expr_list(mut input: Str) -> Result {
    let mut acc = vec![];
    let last;
    while let Ok((a, s)) = {
        if let Ok((tree, mut string)) = parse_norm_expr(input) {
            if !string.check(",") {
                Err(())
            } else {
                Ok((tree, string))
            }
        } else if let Ok((tree, mut string)) = parse_block_expr(input) {
            string.check(",");
            Ok((tree, string))
        } else {
            Err(())
        }
    } {
        input = s;
        acc.push(a);
    }
    match parse_norm_expr(input) {
        Ok((a, s)) => {
            input = s;
            last = Box::new(a);
        }
        Err(e) => match parse_block_expr(input) {
            Ok((a, s)) => {
                input = s;
                last = Box::new(a);
            }
            Err(f) => {
                return Err(ParseError::ExprListEmpty(Box::new(ParseError::Multiple(
                    vec![e, f],
                ))));
            }
        },
    }
    input.check(",");
    let body = if acc.is_empty() { None } else { Some(acc) };
    Ok((AST::ExprList { body, last }, input))
}

// normExpr   <- arithmetic | dataDecl | literal | name
fn parse_norm_expr(input: Str) -> Result {
    let mut err_stack: [std::mem::MaybeUninit<Box<ParseError>>; 4] =
        unsafe { std::mem::MaybeUninit::uninit().assume_init() };
    match parse_arithmetic(input) {
        Err(e) => {
            err_stack[0].write(Box::new(e));
        }
        o => {
            return o;
        }
    }
    match parse_data_decl(input) {
        Err(e) => {
            err_stack[1].write(Box::new(e));
        }
        o => {
            return o;
        }
    }
    match parse_literal(input) {
        Err(e) => {
            err_stack[2].write(Box::new(e));
        }
        o => {
            return o;
        }
    }
    match parse_name(input) {
        Err(e) => {
            err_stack[3].write(Box::new(e));
        }
        o => {
            return o;
        }
    }
    Err(ParseError::NormExprError(unsafe {
        std::mem::transmute(err_stack)
    }))
}

// blockExpr  <- ifStmt | matchStmt | forStmt
fn parse_block_expr(input: Str) -> Result {
    let mut err_stack: [std::mem::MaybeUninit<Box<ParseError>>; 3] =
        unsafe { std::mem::MaybeUninit::uninit().assume_init() };
    match parse_if_stmt(input) {
        Err(e) => {
            err_stack[0].write(Box::new(e));
        }
        o => {
            return o;
        }
    }
    match parse_match_stmt(input) {
        Err(e) => {
            err_stack[1].write(Box::new(e));
        }
        o => {
            return o;
        }
    }
    match parse_for_stmt(input) {
        Err(e) => {
            err_stack[2].write(Box::new(e));
        }
        o => {
            return o;
        }
    }
    Err(ParseError::BlockExprError(unsafe {
        std::mem::transmute(err_stack)
    }))
}
fn parse_if_stmt(input: Str) -> Result {
    return Err(ParseError::TodoError);
    todo!()
}
fn parse_match_stmt(input: Str) -> Result {
    return Err(ParseError::TodoError);
    todo!()
}

// forStmt    <- "for" name "in" normExpr "->" exprList? ("." | ";")
fn parse_for_stmt(mut input: Str) -> Result {
    let ident;
    let source;
    let mut body = None;
    let mut returns = false;
    if !input.check("for") {
        return Err(ParseError::Expected("for"));
    }
    match parse_name(input) {
        Ok((a, s)) => {
            input = s;
            ident = Box::new(a);
        }
        Err(e) => {
            return Err(ParseError::ForStmtIdentError(Box::new(e)));
        }
    }
    if !input.check("in") {
        return Err(ParseError::Expected("in"));
    }
    match parse_norm_expr(input) {
        Ok((a, s)) => {
            input = s;
            source = Box::new(a);
        }
        Err(e) => {
            return Err(ParseError::ForStmtSourceError(Box::new(e)));
        }
    }
    if !input.check("->") {
        return Err(ParseError::Expected("->"));
    }
    if let Ok((a, s)) = parse_expr_list(input) {
        input = s;
        body = Some(Box::new(a));
    }
    match input.check(".") {
        true => {
            returns = true;
        }
        false => {
            if !input.check(";") {
                return Err(ParseError::Multiple(vec![
                    ParseError::Expected("."),
                    ParseError::Expected(";"),
                ]));
            }
        }
    }
    Ok((
        AST::ForStmt {
            ident,
            source,
            body,
            returns,
        },
        input,
    ))
}
fn parse_arithmetic(input: Str) -> Result {
    return Err(ParseError::TodoError);
    todo!()
}
fn parse_data_decl(input: Str) -> Result {
    return Err(ParseError::TodoError);
    todo!()
}

// literal    <- numLit | strLit | charLit | boolLit
fn parse_literal(input: Str) -> Result {
    let mut err_stack: [std::mem::MaybeUninit<Box<ParseError>>; 4] =
        unsafe { std::mem::MaybeUninit::uninit().assume_init() };
    match parse_num_lit(input) {
        Err(e) => {
            err_stack[0].write(Box::new(e));
        }
        o => {
            return o;
        }
    }
    match parse_str_lit(input) {
        Err(e) => {
            err_stack[1].write(Box::new(e));
        }
        o => {
            return o;
        }
    }
    match parse_char_lit(input) {
        Err(e) => {
            err_stack[2].write(Box::new(e));
        }
        o => {
            return o;
        }
    }
    match parse_bool_lit(input) {
        Err(e) => {
            err_stack[3].write(Box::new(e));
        }
        o => {
            return o;
        }
    }
    Err(ParseError::LiteralError(unsafe {
        std::mem::transmute(err_stack)
    }))
}

// numLit     <- ('0'..'9')+
fn parse_num_lit(mut input: Str) -> Result {
    let i = input.chars().take_while(|c| c.is_ascii_digit()).count();
    if i == 0 {
        return Err(ParseError::NumNotDigit);
    }
    // for some god forsaken reason I cannot borrow the underlying
    // string unless its done like this
    let num = &input.string[input.index..input.index + i];
    input.advance(i);
    input.trim_start();
    Ok((AST::Number(num), input))
}

// strLit     <- "\"" (!"\"" ("\\\"" | any))* "\""
fn parse_str_lit(mut input: Str) -> Result {
    let string;
    if !input.check("\"") {
        return Err(ParseError::Expected("\""));
    }
    let mut escaped = false;
    let mut i = 0;
    for c in input.chars() {
        if c == '\\' {
            escaped = true;
        }
        if c == '"' && !escaped {
            break;
        }
        i += 1;
    }
    string = &input.string[input.index..input.index + i];
    input.advance(i);
    if !input.check("\"") {
        return Err(ParseError::Expected("\""));
    }
    Ok((AST::String(string), input))
}

// charLit    <- "'" (!"'" ("\\'" | any)) "'"
fn parse_char_lit(mut input: Str) -> Result {
    let char;
    if !input.check("'") {
        return Err(ParseError::Expected("'"));
    }
    char = match &input.string[input.index..] {
        c if c.starts_with("\\'") => &c[..2],
        c if !c.starts_with("'") => &c[..1],
        _ => {
            return Err(ParseError::CharNotAccepted);
        }
    };
    if !input.check("'") {
        return Err(ParseError::Expected("'"));
    }
    Ok((AST::Char(char), input))
}

// boolLit    <- "true" | "false"
fn parse_bool_lit(mut input: Str) -> Result {
    let b = match &input.string[input.index..] {
        b if b.starts_with("true") => {
            input.advance(4);
            &b[..4]
        }
        b if b.starts_with("false") => {
            input.advance(5);
            &b[5..]
        }
        _ => {
            return Err(ParseError::BollNotBool);
        }
    };
    input.trim_start();
    Ok((AST::Bool(b), input))
}

fn main() {
    let input = Str::new("fn main(args: str): int ->\n\tfor a in args ->\n\t\ta;\n\t0.");
    let r = parse_programm(input);
    println!("{:#?}", r);
}
