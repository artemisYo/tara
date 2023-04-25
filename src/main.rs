mod tokenizer;
use tokenizer::*;

fn main() {
    let s =
    "fn main(argv: [Str]): int -> \n\t10 + 9 == 21.\nfn main2(argv: [Str]): int ->\n\t 10 `add` 9 ` eq` 21.\n";
    println!("{s}");
    println!("----");
    //println!("{:#?}", preparse(tokenize(s).expect("tokenizer failed!")));
    println!("{:?}", tokenize(s).expect("tokenizer failed!"));
}
