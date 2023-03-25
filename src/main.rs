mod parsing;
mod tokenizer;
use crate::tokenizer::tokenize;

fn main() {
    println!(
        "{:#?}",
        tokenize("fn fib(n: int): int {return fib(n-1) + fib(n-2);}")
    );
}
