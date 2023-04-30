mod comb_parser;
mod parser;
mod tokenizer;
//use tokenizer::*;

fn main() {
    // let s =
    // "fn main(argv: [Str]): int -> \n\t10 + 9 == 21\nend\nfn main2(argv: [Str]): int ->\n\t 10 `add` 9 ` eq` 21\nend\n";
    //println!("{s}");
    //println!("----");
    //println!("{:?}", tokenize(s).expect("tokenizer failed!"));
    //println!("{:?}", parser::parse("fn main -> damn end\n"));
    parser::parse("fn main -> damn end\n").unwrap();
}
