Fountain
========

**Fountain** is a work-in-progress grammar formalism capable of expressing
context-sensitive languages (CSLs) and supporting both efficient parsing
_and_ efficient generation of sentential forms of those languages.

It does this by allowing semantic actions to be interspersed between
the terms of a production rule.  Unlike the imperative semantic actions in a
typical parser-generator (such as `yacc`) though, these actions are
relational, and are also called _constraints_.  This situates Fountain
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

    % ./bin/fountain generate eg/anbncn.fountain
    aaaaabbbbbccccc

Neither of the above processes involve backtracking; the string
is parsed or generated in linear time.  However, it is important to note
that, while Fountain supports deterministic, it does not enforce it.
It is possible to write Fountain grammars that lead to backtracking
search, or even infinite loops during generation.  How best to handle
these cases remains an open line of inquiry.

For a fuller description of the Fountain language, see
 **[doc/Definition-of-Fountain.md](doc/Definition-of-Fountain.md)**.

TODO
----

*   A syntax for comments.
*   Terminals should be multi-character in the syntax.
*   Rename "arb" to "param" (?)
*   Allow params to be supplied on command line.
*   Check constraints on all branches of an alternation.
*   Syntax for declaring global variables.
*   Require that variables be declared.
*   Allow variables to be declared with a type.

### To think about

*   Will we want productions to have arguments and how would that work?
*   Will we want productions to have local variables and how would that work?
*   Will we want variables of string type?
*   Will we want variables of "string produced by a certain production" type?

### Aspirational

*   Use Fountain's own parsing facilities to parse the Fountain
    grammar description!  It's not entirely clear to me how much
    of it it could handle.  But it would be close to "writing
    Fountain in Fountain".
