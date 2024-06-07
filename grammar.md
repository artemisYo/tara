# TPEG
Grammar markup is an old struggle. We had BNF, then EBNF, maybe even EEBNF.
We moved on to PEG, various extensions of PEG, and whatever rust's pest library is doing.

But, it'd be nice to have some notion of generic parse rules, so I don't have to type `(a comma)* a?` a gazillion times.

T(ara) PEG is just good old PEG with a few minor additions. 
It is still merely a markup language, so you needn't worry about it.

## Comments
Yeah, sometimes you may want to just explain something rq.
```TPEG
// this is a comment
Foo = :foo
```

## Generic Rules
Functions are good because they're reusable.
Why not do the same with rules?
```TPEG
ListOfFoos = '[' List(Foo) ']'
Foo = "foo"

List(a) = (a ',')* 'a'?
```
Literally just like a template, I can pass a rule or some token to `List(a)` and it will replace each occurence of `a` in it's body with the argument.
The names of the arguments may be annotated at call-site to make it simpler to understand what rules corresponds to.
```TPEG
ListOfFoos = '[' List(element = "foo", separator = ',') ']'
List(element, separator) = (element separator)* element?
```

### Flags
Some parameters may also be flags, aka. boolean values of true/false.
You can then use it with an if statement.
```TPEG
OneFoo = Twice(Foo, false)
TwiceFoo = Twice(Foo, true)

Twice(r, f) = 
	if f 
		then r r 
		else r
```
Here, `Twice(r, f)` is equal to `r r` if `f` is true, otherwise it simply parser `r`.
Alternatively you can lay out the rule multiple times with concrete values to separate definitions:
```TPEG
Twice(r, true) = r r
Twice(r, false) = r
```

## Token Notation
How do you type the string of a number? 
Some tokens can have multiple underlying lexemes, as such it makes little sense to write a parser over tokens in terms of strings.
```TPEG
Array = :bracketOpen List(:number) :bracketClose
List(a) = (a :comma)* a?
```
Tokens start with a colon, then their name. That's it.

## Tree Streams
Since tara's tokens form a tree (where parenthesized content forms a single branch), it is necessary to denote how the lower levels of a branch are handled.
```TPEG
Function = :func :name :paren{Args} :brace{Block}
Args  = (:name :comma)* :name? ε
Block = (Statement :semi)* ε
Statement = ...
```
Here the curly braces after a token name denote which rule to apply to the contents of the branch the token introduces.
The `ε` in a rule means end of stream, which occurs when the branch ends.
I.e. `(a, b)` will become `:paren{:name :comma :name ε}`.

# Tara's Grammar
```TPEG
File = (&:func Func)* ε
Func = :func :name :paren{Args} (:arrRight :name)? :brace{Block}
Args = List(trailing = true, expect-ε = true, Decl, :comma)
Decl = :name :colon :name

Block = (Statement :semi)* Expr? ε
Statement = :let :name :eq Expr
	      / Expr
Expr = BlockExpr / InlineExpr
BlockExpr = :if Expr :brace{Block} (:else :brace{Block})?

InlineExpr = BoolExpr
BoolOp = :less / :more / :eqEq / :lessEq / :moreEq
BoolExpr = Arithmetic (BoolOp Arithmetic)*
Arithmetic = FuncCall ((:plus / :minus) FuncCall)*
FuncCall = LitExpr (:paren{List(false, true, Expr, :comma)})?
LitExpr = :name / :number / :string

List(trailing, expect-ε, r, s) =
	if trailing
	    then (r s)* r?
	    else r? (s r)*
	if expect-ε 
	    then ε
```

