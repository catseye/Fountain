Fountain
========

Cat's Eye Technologies' **Fountain** is a work-in-progress grammar formalism
capable of expressing context-sensitive languages (CSLs), and supporting both
efficient parsing _and_ efficient generation of strings conforming to those
languages.

It does this by allowing semantic actions to be inserted between the
elements of a production rule.  Unlike the imperative semantic actions in a
typical parser-generator (such as `yacc`) however, these actions are
_relational_, and are also called _constraints_.  This situates Fountain
closer to the Definite Clause Grammars (DCGs) or Attribute Grammars (AGs).

To support efficient generation, the interspersed semantic actions
are analyzed to determine a plausible deterministic strategy for generation.

Here is an example Fountain grammar which expresses the classic CSL
`a`^_n_ `b`^_n_ `c`^_n_:

    Goal ::=
        <. a = 0 .> { "a" <. a += 1 .> } <. a = n .>
        <. b = 0 .> { "b" <. b += 1 .> } <. b = n .>
        <. c = 0 .> { "c" <. c += 1 .> } <. c = n .>
        ;

During parsing based on this grammar, the variable `n` will be,
like the others, initially undefined.  The first time `a = n` is
encountered, `a` will be unified with `n`, and will take on its
value.  When `b = n` is later encountered, unification of `b`
with `n` will take place; if `b` is some value other than `n`,
the parse will fail.

    % echo -n "aaabbbccc" | ./bin/fountain parse eg/anbncn.fountain --
    Success

In comparison, during generation, we assume the variable `n` has
already been assigned a value, as part of the input to the generation
process (which may be externally supplied, and generated randomly).
In addition, the repetition construct `{ "a" <. a += 1 .> }` can "see"
the `a = n` constraint that follows it; it will be checked on each
iteration, and the repetition will terminate when it is true.

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

See also **[doc/Fountain-Design-Questions.md](doc/Fountain-Design-Questions.md)**
for many interesting questions that the design of Fountain raises, such as:

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

### Implementation

*   Allow params on command line when parsing, too.
*   Better usage message.

### Documentation

*   Test cases for backtracking during parsing.
*   Test cases for backtracking during generation.

### To think about

*   When parameters are declared, do we also want to declare their types?
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
    pseudo-randomly generated.  But, another major consideration is that
    we don't _really_ want to thread this state through explicitly.  We'd
    like it to be a bit tidier than that.
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

History
-------

### 0.2

0.2 refined some of the core ideas of Fountain.  The
**[Fountain Design Questions](doc/Fountain-Design-Questions.md)**
document was written.  Parameter passing was added to
productions.  Many small improvements were made to the
reference implementation.

0.1 had an `arb` construct, which was intended to signal that
a variable could be computed during parsing, but was needed to
be defined ("arbitrarily") beforehand during generation.  Essentially
it asserted that the variable was defined, but only during generation.
During the design work for 0.2 it was determined that it was not
necessary (this sort of signal overlaps with parameters to a
production, which signal some kind of arguments need to be supplied;
and in another sense, shouldn't need to be stated inside the grammar
because it is the concern of the client of the grammar rather than
the grammar itself), and it was removed.

### 0.1

0.1 was the original release of Fountain, to show proof of concept.
Only global variables were supported.  Efficient choice was not
supported.
