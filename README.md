# SubZero - A Research Language On Coercive Subtype Inference

## Language Overview
[x] Pure functional with (rank 1) polymorphism in the style of HM
[x] Labeled records supporting width- and depth- subtyping
[ ] Polymorphic variants with full inference
[ ] Labeled nominal row variants for defining algebraic datatypes
[ ] Unlabeled variants supporting implicit coercions
[ ] Inheritance system for fine control over row types
[x] Numeral types hierarchy
[ ] User defined coercions

[Trello Board](https://trello.com/b/2mf5ulFs/subzero)

## Acknowledgement
The checker is heavily based on the [SimplerSub](https://github.com/LPTK/simpler-sub) project, which is a simplified version of [SimpleSub](https://github.com/LPTK/simple-sub) introduced by Lionel Parreaux in [his paper](https://dl.acm.org/doi/pdf/10.1145/3409006), extending the work from Stephen Dolan's thesis on [algebraic subtyping](https://www.cs.tufts.edu/~nr/cs257/archive/stephen-dolan/thesis.pdf).
I also took inspiration from [a paper](https://www21.in.tum.de/~nipkow/pubs/aplas11.pdf) by Traytel et al on the subject.

## Rationale
Algebraic subtyping as originally described by Dolan takes an existing type system (such as Hindley Milner) and synthesizes a distributive lattice out of it. This is done by introducing new type formers for union and intersection types, corresponding to joins and meets on the type lattice. That's great for giving wide freedom and dynamic-feeling behavior of programs, but it makes coercions insertion a bit more challenging. Their system can also result in many types which are considered too flexible for a static language, and make it harder to reason about the program at compile-time.

Instead, I decided to ensure a (partial) lattice structure to the base system itself by defining meets and joins which factor through type formers (according to variance) and stop at base types and their atomic coercions. This approach takes a step back from the algebraic foundation, and gives a more structural system. The idea still preserves a lot of the system's inference power, while also allowing easier insertion of coercions. This ends up being very similar to SimplerSub in terms of inference power.

## Future
Algebraic type systems can be thought of as having implicit coercions, in a sense. They automatically coerce values into unions and out of intersections. Making those coercions explicit will require some (probably lite) kind of dataflow analysis, or smarter type representations and stronger coherence conditions. Another difficulty in inserting coercions is lack of concrete solutions for metavariables. This is fine in systems like SimpleSub, where the runtime representation of data is uniform and coherence is free.

I believe it's still possible to support explicit coercions in an algebra-based checker, but that will remain for future research to discover.

Secondly, my current algorithm as implemented here is only probably-correct. I test it on various examples, and try to run as many test cases as I could think of, but it's still not formally verified. A proper verification of the algorithm is something I am very interested in looking into, but leave as a lower priority for now. On top of ensuring correctness, A formal study of the system will allow comparing my algorithm's strength to other existing approaches, and thus help deciding on the best approach for different use cases.