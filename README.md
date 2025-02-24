# Tara — The Compiler
Work was originally done in the `cara` repository, which housed the compiler written in C.
However, as the reason for choosing C was lost and I was slowly getting frustrated with the language,
I chose to start a rust-rewrite for my own sanity.
# Tara — The Language
The aim is a rust-level (if not lower) language, that incorporates an even richer type system a la Haskell. 
The idea was sparked by not being able to write:
```rust
struct Bar<C> {
    baz: usize,
    branch_a: Box<Bar<C>>,
    branch_b: C<Foo>
}
```
That is, rust lacking support for true higher kinded types.
# Documentation
Currently the little documentation that does exist is scattered all over in my personal files, cloud apps, devices, etc...
