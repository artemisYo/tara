mod tokenizer;
use tokenizer::*;

fn main() {
    let s = "operator +.\noperator =.\nfn main(argv: [Str]): int \n\t10 + 9 = 21\n.";
    println!("{s}");
    println!("----");
    println!("{:#?}", preparse(tokenize(s).expect("tokenizer failed!")));
}
