# Algebraic Types Design Space

Algebraic types here are meant in the sense of row types, ie row sums and row products.
The design space has four major parameters to play with
- Sum vs Product
- Structural vs Nominal
- Closed vs Open
- Unlabeled vs Labeled
each configuration of those parameters yields a different design pattern, however not all combinations make sense.
This file enumerates the possibilities and explains them

## Nominal Type Aliases
Any type can be made nominal by creating a newtype alias around it
```
newtype NominalPair a b = (a, b)
```

## Structural Type Aliases
Aliases can also be defined normally. In this case `StructuralPair a b` is completely equivalent to `(a, b)`
```
type StructuralPair a b = (a, b)
```

## Variants (ADTs)
These are traditional ADTs from the ML family of languages.
They have a type name, which might be parameterized, and a list of constructors.

```
[sealed] variant List a with
    | Nil
    | Cons a (List a)
```

non sealed variants can be inherited from
```
[sealed] variant Tree a extending List a with
    | Branch (Tree a) (Tree a)
```

## Labeled Sum Types
These act like union types but also maintain a tag for you.
Here every constructor must take exactly one parameter
```
type Val = { `I int | `S string | `R real }
```
Constructors can be inserted automatically to coerce eg `int <: Val`, or used explicitly as in ``I 42`.

## Unlabeled Sum Types
These are very similar to union types, but their constructors are numeric,
```
type Val = int | string | real
```
Coercions are also automatic, but can be explicit with ``1` or any other index.
Additionally both labeled and unlabeled sums should support row polymorphism.


## Records
nominal records can be defined
```
[sealed] record Pair a b with
    fst : a
    snd : b
```

non sealed records can be inherited from
```
[sealed] record Triplet a b d extending Pair a b with
    thd : c
```
subtype relation on nominal records is not structural, but only inheritence-based

## Labeled Product Types (Records)
Records can also be structural
```
type Triplet a b c = { fst : a; snd : b; thd : c }
```
These are always extensible.

## Unlabeled Product Types (Tuples)
It's also allowed to use tuples as usual
```
type Triplet a b c = (a, b, c)
```

## In Pattern Matching
Pattern matching on product types is always non-refutible.
```
let (x, y, z) = myTriplet
```
But they can also make use of row subtyping
```
let {fst = x; thd = z} = myOtherTriplet
```
Records are nominal and thus don't need to be matched on


Sum types are more interesting to match. For example variants:
```
cases myList
| Nil . branch
| Cons x rest . branch
```
Tagged unions can also be matched on
```
cases myVal
| `I i . printInteger i
| `S s . printString s
```
It's impossible to match on untagged unions, but they're still useful as they support coercions into tagged stuff (they keep an internal tag anyway).