Design of Fountain
==================

Design Goals
------------

Fountain should:

*   Be a grammar formalism (rather than a programming language).
*   Permit implementations that perform both efficient parsing of
    strings, and efficient generation of strings, based on a single
    Fountain grammar.
*   Be able to express all the Context-Sensitive Languages.
*   Be able to express _only_ the Context-Sensitive Languages (lofty goal; see below).
*   Allow these CSLs to be expressed as concisely and cleanly as possible.

Questions
---------

### Can't a Definite Clause Grammar (DCG) do what Fountain does?

It's true that DCGs have a simple relational definition, such that a DCG can be
easily coded in a logic (or relational) programming language such as Prolog
(or miniKanren), and in that form, directly executed.

To the extent that the language supports querying the relation in both directions
(miniKanren is stronger than Prolog in this regard), such a DCG can be used to
both parse and generate strings of its language.

However, resorting to nondeterministic search when processing in one of the
directions, even when the other direction can be processed deterministically,
is apparently unavoidable.  Implementing nondeterministic search efficiently is
well-understood to be extremely challenging.  It does not "scale up" well,
and enumerative approaches tend to enumerate the least interesting results first.

There is ongoing research to implement more sophisticated search strategies in
miniKanren that could be useful here.  But often, tuning the search involves some
kind of annotation or configuration, potentially muddying the exposition of the grammar
and obscuring its structure.  For Fountain, there is a desire to have as
clean a formulation as possible -- one that most resembles the most direct statement
of the problem.

### Doesn't a Context-Sensitive Grammar (CSG) do what Fountain does?

It's true that every CSL can be described with a CSG -- in fact,
the CSLs are defined as the languages accepted by CSGs.  However, the
way that a CSG operates is very different from how Fountain operates.

By CSG I'm referring to "Type-1" grammars in the Chomsky hierarchy.

All grammars in the Chomsky hierarchy are basically string-rewriting systems.

When context is involved in such a rewriting system, that context is
"shuttled around" from production to production during parsing, much
like how the head of a single-tape Turing machine often must traverse
potentially large distances of the tape in order to access data that's
relevant to what it's doing at some other location on the tape.

For a theoretical construct this is fine.  For a practical parser
(resp. generator) though, all this shuttling makes it a really slow way
to parse (resp. generate) strings of a language.

Fountain, on the other hand, much like Attribute Grammars (AGs),
stores its context in an ancillary structure which is essentially
random-access and independent of the linear string of terminals
and nonterminals of the string being parsed or generated.  This makes
it much more efficient as a method for supporting context-sensitivity.

### Isn't Fountain really a programming language in disguise?

Fountain resides in the murky twilight beween programming language and
grammar formalism, as all sufficiently powerful grammar formalisms must.

Using the grammar parts of Fountain feels like constructing a grammar
and defining a syntax; using the constraint parts of Fountain feels
like writing a program.  It's a design goal for it to feel as little
like writing a program as possible.  That is, it shouldn't be the case
that the user is required to do a lot of fiddly detail work, updating
variables and so forth, to achieve the result they desire.  Still, a
certain amount of that is probably inevitable, and I suspect it's not even
well understood how _much_ of it is inevitable.  It might be that a
great deal of fiddly work is required for most interesting tasks, and
that writing Fountain grammars unavoidably feels a lot like programming.
That would be a disappointing result.  The jury's still out though.

One property that we think of most programming languages having is that
they are Turing-complete.  One of Fountain's design goals is that it
can express only the CSLs, and is therefore not Turing-complete; you cannot,
for instance, write a Lisp interpreter in it.
This distances it from previous experiments by Cat's Eye Technologies
to design "grammar-like" programming languages, such as
[Tamsin][] and [Tandem][], which were intended to be Turing-complete.

All the same, parsing a context-sensitive language is a PSPACE-complete problem,
and PSPACE is a huge computational class -- it's known to contain NP.
And NP-complete problems are already generally considered "intractable";
so for all practical purposes, PSPACE ought to be ample.

### How can it be ensured that Fountain can express only the CSLs?

Fountain exists for the purpose of to parsing and generating
context-sensitive languages.  To this end, it is a design goal
of Fountain grammars that they should be able to express
_all_ and _only_ the context-sensitive languages.

It turns out there are a number of snags with this goal, however, so we
treat it as more of a "moral" goal than a practically achievable one.

We will examine these snags in subsections below.

#### Can we not show that Fountain is equivalent to the CSGs?

So, if we could say that for every Fountain grammar there's an equivalent
CSG that accepts and rejects the very same strings, and show how to derive
it mechanically, then we could say Fountain captures exactly the CSLs.

However, Fountain is structured differently enough from the CSGs (see
"shuttling" vs. "ancillary context", above) that this would not be a simple
task.

The structure of Fountain, in fact, closely resembles the structure of
recursive functions.  Productions can pass arguments to other productions,
and productions can refer to themselves.

But even if we restrict the structure that Fountain has here to that of the
primitive recursive functions (PR), we run into a problem when we try to
compare it to the structure of the CSGs.

We will elaborate on this problem in the next subsection.

#### Where do the CSLs even end?

Although the current state of theoretical computer science can say where the
CFLs end and the CSLs begin, it seems to be unable to say exactly where the
CSLs end (without making direct reference to CSGs).

Here's what is known:

*   Parsing a CSG is a PSPACE-complete problem.  That is, the problem of
    taking a description of a CSG and a string and accepting if the
    string is a member of the language and rejecting if it isn't, is in
    PSPACE, and is at least as complex as any other problem in PSPACE.
*   Every CSL is equivalent to a linear-bounded automaton (LBA).  This
    is true even if we don't describe our CSL with a CSG.  An LBA will
    execute in NSPACE(n), where n is the linear bound.  This suggests
    (though I haven't seen it spelled out this way, unfortunately)
    that the problem of simulating an LBA (i.e. take a description of
    an LBA and a string on input, accept iff LBA accepts that string)
    is also PSPACE-complete.  It would make sense if it was.  Parsing
    a CSL is, at any rate, somewhere in PSPACE.
*   Computing a primitive recursive function is NEXPTIME-complete.  That
    is, the problem of taking a description of a PR function and an
    integer and accepting if that function computes that integer and
    rejecting if it doesn't, is in NEXPTIME, and is at least as complex
    as any other problem in NEXPTIME.
*   PSPACE is known to be contained in NEXPTIME, but the containment
    is not known to be strict.
*   Therefore we don't know if there are any problems that are in
    NEXPTIME that aren't in PSPACE.
*   Therefore we don't know if there are any PR functions which can be
    expressed as a CSL. (Because if there were, we could seperate
    NEXPTIME and PSPACE, and we haven't.)

Therefore we don't have an indication of which PR functions are
expressible as CSLs, and which aren't.

Therefore we don't have a good way to look at a Fountain grammar and
say if it describes a CSL, or something outside the CSLs (for example,
a primitive recursive function).

#### Is generating a string that is a member of a CSL also in PSPACE?

Honestly, I don't know.  I haven't come across anything about this in
the literature yet.

My guess is that it is, based on the following reasoning.  An acceptor
for a CSL can be turned into a parser for that CSL, one which outputs
some of its context (e.g. outputting `n=3` when given `aaabbbccc`),
efficiently.  This parser can also be rephrased as a transducer
(e.g. it outputs `aaabbbccc` when given `n=3`; this sort of thing is
exactly Fountain's purpose).  This reverse "generation" process is
no more and no less fundamentally complex than the original "parsing"
process.  So it too is in PSPACE.

But that's a guess.  I don't know.

It could also be the case that the generation goals are not sufficiently defined,
and Fountain keeps searching forever for a satisfying string, and never finds
one.  In that case, we'd need to weaken our complexity claim to something like
"When generating a string terminates, it uses only polynomial space".

#### What if we restrict the recursion that Fountain is capable of, in some way?

Even if we limit the recursion to well-founded recursion (such
as found in [Exanoke][]), primitive recursive computations can
still be expressed.  Primitive recursion is known to be able to
solve any problem in NEXPTIME.

Forbidding recursion entirely might help -- so long as we also confirm
that every instance of every repetition construct necessarily consumes one
or more tokens from the input string, to prevent unbounded looping.

But expressing grammatical structures such as nested parenthesis using
repetition alone might be inconvenient to the point of being obnoxious.

And it remains unclear if it would help us show that the parsing process
can be captured by a CSL and does not allow the equivalent of PR functions
to be constructed.

### Why would we want to support local variables?

Say we want to parse any amounts of whitespace between tokens, but
when generating, always generate a fixed amount of whitespace.  We
can't do this with a global variable, because then we would always
have to have the _same_ amount of whitespace between any two tokens.

We want a local variable.  Moreover, we always want to unify it with
1 when generating.

We don't need to explicitly declare a local variable, we just start
using it.  So our "space" production might look something like:

    Space ::= <. n = 0 .> { " " <. n += 1 .> } <. n > 0 .>

### How can parameter passing be implemented?

All parameters are "reference parameters" in some sense (think Prolog).

All productions only have access to their own parameters -- there are
no global variables.

When we encounter a nonterminal (during, let's say, parsing), we need
to

*   Save our current store somewhere.  Doesn't have to be in the
    parsing state.  Though we could just keep a copy of the parsing
    state.
*   Create a new store for the nonterminal we're descending into.
*   Look at the formals of the nonterminal we're descending into.
*   For each of these formals, see if we have an actual that gives
    it a value.  If we do, give it that value in the new store.
*   It's important we let some formals stay undefined though, as
    the nonterminal we're descending into might assign them and
    pass them back up as "output parameters".
*   Create a new copy of the parsing state that has our new store.
*   Parse the nonterminal with it.  The result will be a new
    parsing state.
*   Reconcile the new parsing state and the saved parsing state.
    We use everything from the new parsing state unchanged,
    except for the store.  The store, we reconcile with our
    saved store.
*   What does "reconcile" mean?  For every formal that (now)
    has a value in our new parsing state, unify it with the
    actual that we used to populate the formal.  This will
    result in a new store where the actuals may be associated
    with values different than what they were.  Use this new store
    in the new parsing state.

[Exanoke]: https://catseye.tc/node/Exanoke
[Tamsin]: https://catseye.tc/node/Tamsin
[Tandem]: https://catseye.tc/node/Tandem
