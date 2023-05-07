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

See also the **[doc/Fountain-Design-Questions.md]()** for many interesting
questions that the design of Fountain raises, such as:

*   Can't a Definite Clause Grammar (DCG) do what Fountain does?
*   Doesn't a Context-Sensitive Grammar (CSG) do what Fountain does?
*   Isn't Fountain really a programming language in disguise?
*   What complexity class does Fountain aim to capture?
*   Is generating a string that is a member of a CSL also in PSPACE?
*   How does Fountain ensure no Fountain grammar strays outside PSPACE?
*   What if we restrict Fountain to well-founded recursion?
*   What if we forbid recursion entirely?

TODO
----

### Syntax

*   A syntax for comments.
*   A syntax for terminals so that `"` can be given as a terminal.
    Probably any unicode code point by its hex.

### Semantics

*   Inc, dec, gt, lt, should take either a variable or an integer on the RHS.
    Really, the RHS could be some kind of simple expression probably.
*   Check constraints on all branches of an alternation.
*   Allow local variables to be declared.

This last item comes from the following use case: say we want to parse any amounts
of whitespace between tokens, but when generating, always generate a fixed amount of
whitespace.  We can't do this with a global variable (because then we always have to
have the _same_ amount of whitespace between any two tokens).  We want a local
variable.  Moreover we always want to unify it with 1 when generating.

So our "space" production looks something like:

    Space ::= <. local n = 0 .> { " " <. n += 1 .> } <. n > 0 .>

### Implementation

*   Allow params on command line when parsing, too.
*   Better usage message.

### Documentation

*   Test cases for backtracking during parsing.
*   Test cases for backtracking during generation.

### To think about

*   Do we really want to allow `arb` binding to have a default value?
*   Do we actually even need `arb`?  I thought we did, but...?
*   Do we want to require global variables to be declared?  With types?
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
