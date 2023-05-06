Fountain
========

Cat's Eye Technologies' **Fountain** is a grammar formalism capable of
expressing context-sensitive languages (CSLs) and supporting both efficient
parsing _and_ efficient generation of strings conforming to those languages.

It does this by allowing semantic actions to be inserted between the
elements of a production rule.  Unlike the imperative semantic actions in a
typical parser-generator (such as `yacc`) however, these actions are
_relational_, and are also called _constraints_.  This situates Fountain
closer to the Definite Clause Grammars (DCGs) or Attribute Grammars (AGs).

To support efficient generation, the interspersed semantic actions
are analyzed to determine a plausible deterministic strategy for generation.

Here is an example Fountain grammar which expresses the classic CSL
`a`^_n_ `b`^_n_ `c`^_n_:

    Goal ::= <. arb n .>
             <. a = 0 .> { "a" <. a += 1 .> } <. a = n .>
             <. b = 0 .> { "b" <. b += 1 .> } <. b = n .>
             <. c = 0 .> { "c" <. c += 1 .> } <. c = n .>
             ;

During parsing based on this grammar, the `arb n` constraint is
ignored and leaves `a` undefined.  The first time `a = n` is
encountered, `a` will be unified with `n`, and will take on its
value.  When `b = n` is later encountered, unification of `b`
with `n` will take place; if `b` is some value other than `n`,
the parse will fail.

    % echo -n "aaabbbccc" | ./bin/fountain parse eg/anbncn.fountain --
    Success

In comparison, during generation, `arb n` will cause `n` to take on
an arbitrary (for example, random) value.  In addition, the
repetition construct `{ "a" <. a += 1 .> }` can "see" the
following `a = n` constraint, will check it on each iteration,
and will terminate when it is true.

    % ./bin/fountain generate eg/anbncn.fountain n=5
    aaaaabbbbbccccc

Neither of the above processes involve backtracking; the string
is parsed or generated in linear time.  However, it is important to note
that, while Fountain supports deterministic operation, it does not enforce it.
It is possible to write Fountain grammars that lead to backtracking
search, or even infinite loops during generation.  How best to handle
these cases remains an open line of inquiry.

For a fuller description of the Fountain language, see
 **[doc/Definition-of-Fountain.md](doc/Definition-of-Fountain.md)**.

### Can't a Definite Clause Grammar (DCG) do this?

It's true that DCGs have a simple relational definition that can be embedded
in a logic (or relational) programming language such as Prolog (or miniKanren),
and this directly executed.

To the extent that the language supports querying the relation in both directions
(miniKanren is stronger than Prolog in this regard), such a DCG can be used to
both parse and generate strings of its language.

However, resorting to nondeterministic search when processing in one of the
directions, even when the other direction is deterministically processable,
is seemingly inevitable.  Implementing nondeterministic search efficiently is
well-understood to be extremely challenging.  It does not "scale up" well,
and iterative approaches tend to yield the least interesting results first.

There is ongoing research to implement more sophisticated search strategies in
miniKanren that could be useful here.  But even where tuning the search is a
possibility, there is a desire to have as clean a formulation as possible -- one
that most resembles the most direct statement of the problem.  Fountain aims for
having this sort of clean formulation.

### Isn't this really a programming language?

Fountain resides in the murky twilight beween programming language and
grammar formalism, as all sufficiently powerful grammar formalisms must.

Parsing a context-sensitive language is a PSPACE-complete problem, and
being able to express all, and only, the context-sensitive languages is
a design goal of Fountain (whether it achieves it perfectly or not).

There are many problems that cannot be solved in PSPACE, for example
computing the Ackermann function.  So Fountain is definitely not
Turing-complete; you won't be able to write a Lisp interpreter in it.
This distances it from previous experiments by Cat's Eye Technologies
to design "grammar-like" programming languages, such as
[Tamsin][] and [Tandem][], which were intended to be Turing-complete.

At the same time, PSPACE is a huge computational class, one which is
known to contain NP, and is thought to be quite a bit larger.
NP-complete problems already are generally considered "intractable";
so for all practical purposes, PSPACE ought to be ample.

Preventing Fountain from expressing languages which are not
context-sensitive is still an open line of inquiry.  Clearly we
must disallow unrestricted recursion, but the best way of doing
this is less clear.

Even if we limit the recursion to well-founded recursion (such
as found in [Exanoke][]), primitive recursive computations can
still be expressed.  Primitive recursion is known to be able to
solve any problem in NEXPTIME, which is not known to be equal to
PSPACE, and is suspected to be larger.

Forbidding recursion entirely will certainly prevent going outside
of PSPACE -- so long as we also confirm that every instance of
every repetition construct necessarily consumes one or more tokens
from the input string.  But expressing grammatical structures such
as nested parenthesis using repetition alone might be inconvenient
to the point of being obnoxious.  And we need to be careful to still
allow all CSLs to be expressed, which might prove cumbersome to show.

TODO
----

### Syntax

*   A syntax for comments.
*   Syntax for declaring global variables (with a type?).

### Semantics

*   Require that variables be declared.  (Unless maybe operating in some cavalier mode)
*   Check constraints on all branches of an alternation.
*   Allow local variables to be declared.
*   Allow `arb` binding to have a default value, as this is possibly the only thing that makes sense for local variables.

The last two items come from the following use case: say we want to parse any amounts
of whitespace between tokens, but when generating, always generate a fixed amount of
whitespace.  We can't do this with a global variable (because then we always have to
have the _same_ amount of whitespace between any two tokens).  We want a local
variable.  Moreover we always want to unify it with 1 when generating.

So our "space" production looks something like:

    Space ::= <. local n = 0 .> { " " <. n += 1 .> } <. arb n (1) .>

The default could also be an expression based on globals.  Allowing the user to
specify what the spacing is every time the Space production is generated.

### Implementation

*   Allow params on command line when parsing, too.
*   `roundtrip` command that generates then parses, to confirm correct operation.

### Documentation

*   Test cases for backtracking during parsing.
*   Test cases for backtracking during generation.

### To think about

*   Will we want productions to have arguments and how would that work?
*   Will we want variables of string type?
*   Will we want variables of "string produced by a certain production" type?
*   What other types might we want?  Lists and maps and sets seem likely.

### Aspirational

*   Allow (pseudo)random numbers to be used in generation.
    Probably we can have a built-in function that takes a seed a produces
    the next pseudorandom number in the sequence.  And another function for
    limiting that number to a desirable range (i.e. modulo).
    The key here is that we must also be able to parse what we've
    pseudo-randomly generated.
*   Write the "kennel story" generator in Fountain.  Show that
    it can parse the same story it generated, in a reasonable
    time, even up to 50,000 words.
*   Use Fountain's own parsing facilities to parse the Fountain
    grammar description!  It's not entirely clear to me how much
    of it it could handle.  But it would be close to "writing
    Fountain in Fountain".
*   Report error diagnostics (i.e. what caused a failure).  My
    concern is that this will make the structure of the
    implementation more cloudy.

[Exanoke]: https://catseye.tc/node/Exanoke
[Tamsin]: https://catseye.tc/node/Tamsin
[Tandem]: https://catseye.tc/node/Tandem
