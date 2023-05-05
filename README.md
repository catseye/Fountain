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

TODO
----

### Syntax

*   A syntax for comments.
*   Terminals should be multi-character in the syntax.
*   Rename "arb" to "param" (?)
*   Syntax for declaring global variables (with a type).

### Semantics

*   Require that variables be declared.  (Unless maybe operating in some cavalier mode)
*   Check constraints on all branches of an alternation.

### Documentation

*   Design goals (talk about PSPACE, etc).
*   Test cases for backtracking during parsing.
*   Test cases for backtracking during generation.

### To think about

*   Will we want productions to have arguments and how would that work?
*   Will we want productions to have local variables and how would that work?
*   Will we want variables of string type?
*   Will we want variables of "string produced by a certain production" type?
*   What other types might we want?  Lists and maps and sets seem likely.

### Aspirational

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
