use crate::parser::*;

// blockExpr  <- ifStmt | matchStmt | forStmt | "(" exprList ")"
pub fn parse_block_expr(mut input: Str) -> Result {
    orig!(BlockExpr);
    let mut err_stack = vec![];
    match parse_if_stmt(input) {
        Err(e) => {
            err_stack.push(e);
        }
        o => {
            return o;
        }
    }
    match parse_match_stmt(input) {
        Err(e) => {
            err_stack.push(e);
        }
        o => {
            return o;
        }
    }
    match parse_for_stmt(input) {
        Err(e) => {
            err_stack.push(e);
        }
        o => {
            return o;
        }
    }
    if input.check("(") {
        let exprlist;
        match parse_expr_list(input) {
            Ok((a, s)) => {
                input = s;
                exprlist = a;
                if input.check(")") {
                    return Ok((exprlist, input));
                } else {
                    err_stack.push(ParseError::Meta {
                        origin,
                        loc: input,
                        err: Box::new(ParseError::Expected(")")),
                    });
                }
            }
            Err(e) => {
                err_stack.push(e);
            }
        }
    } else {
        err_stack.push(ParseError::Meta {
            origin,
            loc: input,
            err: Box::new(ParseError::Expected("(")),
        })
    }

    Err(ParseError::Multiple {
        origin,
        err: err_stack,
    })
}

// ifStmt     <- "if" exprList block ("else" block)?
fn parse_if_stmt(mut input: Str) -> Result {
    orig!(IfStmt);
    let condition;
    let smash;
    let mut pass = None;
    if !input.check("if") {
        return Err(ParseError::Meta {
            origin,
            loc: input,
            err: Box::new(ParseError::Expected("if")),
        });
    }
    match parse_expr_list(input) {
        Ok((a, s)) => {
            input = s;
            condition = Box::new(a);
        }
        Err(e) => {
            return Err(ParseError::Trace {
                origin,
                err: Box::new(e),
            });
        }
    }
    match parse_block(input) {
        Ok((a, s)) => {
            input = s;
            smash = Box::new(a);
        }
        Err(e) => {
            return Err(ParseError::Trace {
                origin,
                err: Box::new(e),
            });
        }
    }
    if input.check("else") {
        match parse_block(input) {
            Ok((a, s)) => {
                input = s;
                pass = Some(Box::new(a));
            }
            Err(e) => {
                return Err(ParseError::Trace {
                    origin,
                    err: Box::new(e),
                });
            }
        }
    }
    Ok((
        AST::IfStmt {
            condition,
            smash,
            pass,
        },
        input,
    ))
}

// matchStmt  <- "match" exprList matchPatt+
fn parse_match_stmt(mut input: Str) -> Result {
    orig!(MatchStmt);
    let source;
    let mut patterns = vec![];
    if !input.check("match") {
        return Err(ParseError::Meta {
            origin,
            loc: input,
            err: Box::new(ParseError::Expected("match")),
        });
    }
    match parse_expr_list(input) {
        Ok((a, s)) => {
            input = s;
            source = Box::new(a);
        }
        Err(e) => {
            return Err(ParseError::Trace {
                origin,
                err: Box::new(e),
            });
        }
    }
    match parse_match_patt(input) {
        Ok((a, s)) => {
            input = s;
            patterns.push(a);
        }
        Err(e) => {
            return Err(ParseError::Trace {
                origin,
                err: Box::new(e),
            });
        }
    }
    while let Ok((a, s)) = parse_match_patt(input) {
        input = s;
        patterns.push(a);
    }
    Ok((AST::MatchStmt { source, patterns }, input))
}

// matchPatt  <- "case" (name | exprList) block
fn parse_match_patt(mut input: Str) -> Result {
    orig!(MatchPatt);
    let pattern;
    let smash;
    if !input.check("case") {
        return Err(ParseError::Meta {
            origin,
            loc: input,
            err: Box::new(ParseError::Expected("case")),
        });
    }
    match parse_name(input) {
        Ok((a, s)) => {
            input = s;
            pattern = Box::new(a);
        }
        Err(e) => match parse_expr_list(input) {
            Ok((a, s)) => {
                input = s;
                pattern = Box::new(a);
            }
            Err(f) => {
                return Err(ParseError::Multiple {
                    origin,
                    err: vec![e, f],
                });
            }
        },
    }
    match parse_block(input) {
        Ok((a, s)) => {
            input = s;
            smash = Box::new(a);
        }
        Err(e) => {
            return Err(ParseError::Trace {
                origin,
                err: Box::new(e),
            });
        }
    }
    Ok((AST::MatchPatt { pattern, smash }, input))
}

// forStmt    <- "for" name "in" normExpr block
fn parse_for_stmt(mut input: Str) -> Result {
    orig!(ForStmt);
    let ident;
    let source;
    let block;
    if !input.check("for") {
        return Err(ParseError::Meta {
            origin,
            loc: input,
            err: Box::new(ParseError::Expected("for")),
        });
    }
    match parse_name(input) {
        Ok((a, s)) => {
            input = s;
            ident = Box::new(a);
        }
        Err(e) => {
            return Err(ParseError::Trace {
                origin,
                err: Box::new(e),
            });
        }
    }
    if !input.check("in") {
        return Err(ParseError::Meta {
            origin,
            loc: input,
            err: Box::new(ParseError::Expected("in")),
        });
    }
    match parse_norm_expr(input) {
        Ok((a, s)) => {
            input = s;
            source = Box::new(a);
        }
        Err(e) => {
            return Err(ParseError::Trace {
                origin,
                err: Box::new(e),
            });
        }
    }
    match parse_block(input) {
        Ok((a, s)) => {
            input = s;
            block = Box::new(a);
        }
        Err(e) => {
            return Err(ParseError::Trace {
                origin,
                err: Box::new(e),
            });
        }
    }
    Ok((
        AST::ForStmt {
            ident,
            source,
            block,
        },
        input,
    ))
}
