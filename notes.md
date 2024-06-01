# Some Notes on the Design of Tara
Tara is supposed to be capable of relatively high performance,
thus I want to avoid inducing runtime overhead like a GC or
unnecessary indirection like persistent datastructures.

## Value Semantics / Affine Types
Tara aims to support reasoning about variables in a principled manner.
However complexity associated with tracking of lifetimes references' for 
ensuring validity is quite a big burden.
As such tara instead bets on value semantics, i.e. no references whatsoever.
Data is move-by-default and any modifications by other functions need to be
communicated by returning the modified version of the object.
This is no different from languages like Haskell, except for the fact that
the older version of the value becomes void and unusable:
```tara
func increment(x: int) int { x + 1 }

func main() {
    let x = 5;
    let x = increment(x); // → x = 6
    let y = 4;
    increment(y);
    // foo(y); is invalid now
}
```

This does not bar mechanisms like `Copy` traits from allowing copy-by-default
for certain types.

## Opt-In Linear Types
In some cases it is desirable for API-designers to ensure something is done
with a value, i.e. it is used at least once. 
While that is relatively rare it could be grounds for supporting a mechanism
(similar —but opposite— to Rust's `Copy`)
to allow placing requirement of usage on a type
(Rust offers the attribute `#[must_use]`, which is similar).
```tara
type Foo {
	Bar: Foo;
}
// get it? not clumsy? no implicit Drops?
implement !Clumsy(Foo) {}

func main() {
	let foo = Foo.Bar;
	consumeFoo(foo); 
	// shows error if foo unused.
}
```

## Named Function Parameters
Named function parameters, the caveat being the later discussed
partial function application making the semantics a bit more complex.
A function has a type along the lines of:
```tara
func foo(bar: int, baz: bool) -> int {...}
// → foo: (int, bool) -> int
```
but with named parameters, the type is adjusted to reflect that,
while still being congruent to the previous type:
```tara
func foo(first bar: int, second baz: bool) -> int {...}
// → foo: (first: int, second: bool) -> int
func higherOrderFunction(f: (int, bool) -> int) -> int {...}

func main() { higherOrderFunction(foo); }
```

## Data Constructors
Values of types are constructed using data constructors. 
They look like Agda's data constructors: Types are declared
with associated function signatures, without any implementation.
Data constructors may also choose to not name fields,
in which case they follow the same treatment that fields in tuples do.
```tara
type Foo {
    MakeFoo: (bar: int, baz: int) -> Foo;
}

type Bar {
    Bar: (int, int) -> Bar;
}

func main() {
    let myFoo = Foo.MakeFoo( bar: 5, baz: 1 );
    myFoo.bar + myFoo.baz; // → 6
    let myBar = Bar.Bar(5, 8);
    myBar.0 + myBar.1; // → 13
}
```
FIXME: I don't quite like `bar.0` syntax.

Enums are modelled by multiple constructors:
```tara
type Color {
    Red: Color;
    Blue: Color;
    Green: Color;
    Other: (r: u8, g: u8, b: u8) -> Color;
}

func main() {
    let myColor = Color.Red;
    let myColor = Color.Other(0xff, 0xff, 0);
}
```

## Pattern Matching
Pattern matching deconstructs values. 
Multiple constructors prevent pattern matching in cases 
where only a single pattern is accepted,
such as variable declarations,
otherwise patterns are checked for exhaustiveness:
```tara
type Color { 
    Red: Color; 
    Blue: Color; 
    Green: Color;
    Other: [u8;3] -> Color;
}
func hex(c: Color) -> String {
    case c {
        Color.Red -> "FF0000",
        Color.Blue -> "0000FF",
        Color.Green -> "00FF00",
        Color.Other([r, g, b]) -> {
            fmt("{:X}{:X}{:X}", r, g, b)
        },
    }
}
func toplevelHex(Color.Red: Color) -> String { "FF0000" }
func toplevelHex(Color.Blue: Color) -> String { "0000FF" }
func toplevelHex(Color.Green: Color) -> String { "00FF00" }
func toplevelHex(Color.Other(v): Color) -> String { 
    fmt("{:X}{:X}{:X}", v[0], v[1], v[2]) 
}
```
FIXME: Idk yet if I like `v[index]`.

## Partial Function Application
Should have no runtime overhead when not used, 
excluding at most binary sizes.
Done by generating a struct of the function's arguments whenever
they are only partially supplied (implicit closure).
```tara
func add(lhs: int, rhs: int) -> int {...}

func main() {
    let add5 = add(5);
    // ≡ let manualAdd5 = AddArgStruct( lhs: 5, rhs: null );

    let result = add5(1); // → 6
    // ≡ let manualResult = add(manualAdd5.lhs, 1);
}
```
Functions are applied in their argument order,
unless specifically rearranged using named parameters.
```tara
func pow(base base: int, exponent exp: int) -> int {...}

func main() {
    let square = pow(exponent: 2);
    // ≡ let manualSquare = PowArgStruct( base: null, exponent: 2 );

    let result = square(5); // → 25
    // ≡ let manualResult = pow(5, manualSquare.exponent);
}
```
### Dynamic Dispatch
Since partially applied function have extra information associated,
they can not be passed around for (pratically) free as function pointers.
Thus they behave similarly to rust's closures 
(which they are effectively a specialisation of)
in the requirement of monomorphised generics or dynamic dispatch 
when used in higher order functions or stored in structs.

## Type Constructors
Just like in functional languages, 
types are generalised to generics by type constructors.
Type constructors are just functions on the type level, 
however they do run entirely at compile-time. 
Thus partial application (just like in Haskell) and 
named parameters are possible:
```tara
type Pair(first a, second b) {
    Pair: (a, b) -> Pair(a, b);
}
// → Pair: (first: Type, second: Type) -> Type
```

## Typeclasses
Mostly like in haskell, but with the added benefit of being able to
declare implementors to have a type constructor with named arguments
or to accept out-of-order partially applied type constructors.
This helps us avoid having to create wrapper types just to implement
certain typeclasses in a specific way:
```tara
class Functor(f: (Type) -> Type) {
    map: (a -> b, f(a)) -> f(b);
}
type Pair(first a, second b) {
    Pair: a -> b -> Pair(a, b);
}

implement Functor( Pair(second: x) ) {
    func map(f: a -> b, pair: Pair(a, x)) -> Pair(b, x) {
        let (fst, snd) = pair;
        return Pair(f(fst), snd);
    }
}
```
FIXME: Having a `func name(args) rettype` and `type -> type -> type`
way to write functions is inconsistent and noticeable with typeclasses.

FIXME: With the above example of typeclasses the type parameters `a` and `b`
as well as the implementations `b` are not declared anywhere.

FIXME: `Functor(Pair(second: b))` is hard to parse for readers.

## Subtyping
A notion of subtyping could be supported, 
by allowing types to be declared inside of types.
This means that types form an unbounded semilattice, 
with the join operation being upcasting:
```tara
type Color {
    type Red { Red: Red; }
    type Blue { Blue: Blue; }
    type Green { Green: Green; }
}

func onlyAcceptsRed(red: Color.Red) {...}
func acceptsAllColors(c: Color) {...}

func main() {
    let clearlyRed = Color.Red.Red;
    onlyAcceptsRed(clearlyRed); // valid
    acceptsAllColors(clearlyRed); 
    // clearlyRed is of type Red and thus also of Color
}
```
FIXME: Idk about those nestes constructors tho. 

This allows the type system to also 'remember' the value of variables:
```tara
type Foo {
    type Bar { Bar: (x: int) -> Bar; }
    type Baz { Baz: (b: bool) -> Baz; }
}

func returnsBar() Foo.Bar {
    Foo.Bar.Bar(5)
}

func main() {
    let foo = returnsBar();
    foo.x + 1; // → 6
    // valid as foo is not just Foo but Foo.Bar and 
    // thus has only one possible constructor
    let foo = if someUnforeseenBool {
        Foo.Bar.Bar(1)
    } else {
        Foo.Baz.Baz(false)
    };
    // foo.b;
    // invalid access as Foo.Bar ∨ Foo.Baz = Foo
}
```
The internal Layout of subtypes remains the same,
even if they are declared as normal variants.
This is to avoid unnecessary data movement costs when upcasting,
as these subtypes are not intended to stand on their own.

## Type Combinators
Certain generic types, such as tuples, 
can sometimes be seen as an alternative to structs, 
as they offer a similar ability to declare types,
however without requiring an explicit name to be bound.
As such one could view them as a form of type combinators:
`(a, b)` can be written inside of function declarations,
as a form of "anonymous type". 
However support for extended types like `(a, b, c, d)` 
is up to some mechanism of ad-hoc generation of 4-tuple type declarations,
or manually declaring them in some prelude.

Instead this could be extended to a full notion of combinators,
which leads to the idea of variadic combinators:
since `(a, (b, c))` and `((a, b), c)` are equivalent, this can be seen as an 
associative operation, with some nominal form `(a, b, c)`,
that allows even easier access than either — `x: (a, b, c) -> x.0, x.1, x.2`.
Additionally anonymously tagged unions come to mind:
`a | b` —as well as it's variadic continuations— can become a combinator
for quickly declaring enums. 
The tags could then be named the same way a tuple's fields are:
```tara
func match(x: int | bool | String) String {
    case x {
        0(_) -> "it's a number!",
        1(_) -> "it's a bool.",
        2(_) -> "it's a string :/",
    }
}
```
FIXME: `0(_)` is probably not good for the compiler.

Or even admitting some mechanism to give the tags names:
```tara
func match(x: (Number: int) | (Bool: bool) | (Text: String) ) String {
    case x {
        Number(_) -> ...,
        Bool(_) -> ...,
        Text(_) -> ...,
    }
}
func tuples( x: ((num: int), (b: bool), (text: String)) ) {
    if x.b { 
        fmt("{}", x.num) 
    } else {
        x.text
    }
}
```
FIXME: I do not quite like this way of naming the tags.
## Further Ideas
### Modules/Namespaces as Structs
Like the title; Modules/Namespaces being represented by structs,
kinda like in zig.

### Readonly References or maybe Freezing Values

### Mixfix Operators
Mixfix operators, or generally support for custom operators,
provides the ability to write quite concise code. 
However language support for them is rather complicated, as that
requires a sophisticated runtime dependent grammar scheme.
It is currently not planned; Hopefully at some point it will be.

### Custom Syntactic Constructs
Like Haskell's `do`-notation, it would be nice to offer some mechanism
by which to extend the language to handle certain patterns more ergonomically.
I have no idea how such a mechanism would even work, but javascript's
template strings (like htm as a replacement for jsx) do come to mind.
