mod scoped_map;

mod ast;
mod ir;
mod lexer;
use ast::{exec, syntax, typing};

fn main() {
    let file = std::fs::read_to_string("./test.tara").unwrap();
    println!("----Src----");
    println!("{:?}", file);
    let ts = lexer::tokenize(&file).unwrap();
    println!("----TkS----");
    println!("{:?}", ts);
    let mut st = syntax::parse(&ts).unwrap();
    println!("----AST----");
    println!("{:?}", st);
    typing::do_typing(&mut st);
    let rs = exec::run(st);
    println!("----Run----");
    println!("{:?}", rs);
}
