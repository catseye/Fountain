Fountain
========

_Version 0.4_

Cat's Eye Technologies' **Fountain** is a work-in-progress grammar formalism
capable of expressing context-sensitive languages (CSLs), and supporting both
efficient parsing _and_ efficient generation of strings conforming to those
languages.

It does this by allowing semantic constraints to be inserted between the
elements of a production rule.  To support efficient generation, these
interspersed semantic constraints can be analyzed to determine a usable
deterministic strategy for generation.

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
already been assigned a value, as part of the (externally supplied)
input to the generation process.
In addition, the repetition construct `{ "a" <. a += 1 .> }` can "see"
the `a = n` constraint that follows it; it will be checked on each
iteration, and the repetition will terminate when it is true.

    % ./bin/fountain generate eg/anbncn.fountain n=5
    aaaaabbbbbccccc

Neither of the above processes involve backtracking; the string
is parsed and generated in linear time.  Note, however, that while
Fountain supports deterministic operation, it does not enforce it.
It is possible to write Fountain grammars that lead to backtracking
search, or even infinite loops during generation.  How best to handle
these cases remains an open line of inquiry.

For a more definitive description of the Fountain language, see
 **[doc/Definition-of-Fountain.md](doc/Definition-of-Fountain.md)**.

For insight into the design choices underlying Fountain, see
**[doc/Design-of-Fountain.md](doc/Design-of-Fountain.md)**.  This includes
several interesting questions that the design of Fountain raises, such as:

*   Can't a Definite Clause Grammar (DCG) do what Fountain does?
*   Doesn't a Context-Sensitive Grammar (CSG) do what Fountain does?
*   Isn't Fountain really a programming language in disguise?
*   How can it be ensured that Fountain can express only the CSLs?
*   Should we think of constraints as relational operators?
*   Why would we want to support local variables?
*   How can parameter passing be implemented?
*   How can we apply randomness during generation?
*   How should parameters with different data types be handled?

TODO
----

### Semantics

*   Params on top-level Goal mean those values must be provided from environment.

### Implementation

*   Allow `--` for text-file to take the text to be parsed from stdin.
*   Improved pretty-printing (coalesced terminals, no unnecessary parens, etc.)

### Aspirational

*   Allow "fuel" to be tracked, to ensure the storage is bounded
    linearly to the amount of input.  This aims to ensure that the
    grammar is contained within PSPACE.
*   Use Fountain's own parsing facilities to parse the Fountain
    grammar description!  It's not entirely clear to me how much
    of it it could handle.  But it would be close to "writing
    Fountain in Fountain".
*   Report error diagnostics (i.e. what caused a failure).  My
    concern is that this will make the structure of the
    implementation more cloudy.

History
-------

### 0.4

#### Language

Fountain now distinguishes between productions that may backtrack and
ones that may not, in both parsing and generation.

By default, backtracking is not permitted.  Productions may be marked
with `(*)` to indicate that backtracking is permitted when processing
alternations in that production.

Note however, that backtracking currently has problems, and enabling
it is disabled.

When backtracking is not permitted, every choice in an alternation
must start with a constraint.  Exactly zero or one of those constraints
must be satisfied in a given state.  If none are, that is a failure (and
if this failure happens in an enclosing context where backtracking *is*
permitted, then backtracking will occur in that context).  If more than
one are, the process will abort with an error message.

Also:

Greater than or equal and less than or equal constraints.

"Both" combinator on constraints (which really needs reworking).

#### Implementation

The `--start-symbol` option may now be passed to `fountain` to
cause it to start parsing or generating at the named non-terminal.

Implementation improvements such as better flattening of the AST
representing the grammar during its parsing, and pretty-printing
fragments of the AST (albeit crudely) when displaying them.

### 0.3

Comments and characters terminals given by Unicode code point were
added to the Fountain syntax.

Inc, dec, greater than and less than constraints can take a simple
expression (an integer literal or a variable name) on the right-hand
side.

When parsing, parameters can also be supplied from external sources.

Example Fountain grammar describing a heartwarming and
context-sensitive novel about a wayward animal companion has been
included in the distribution.

Distribution placed under BSD license.

### 0.2

0.2 refined some of the core ideas of Fountain.  The
**[Design of Fountain](doc/Design-of-Fountain.md)**
document (consisting primarily of design questions rather
than design answers) was written.  Parameter passing was added to
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
