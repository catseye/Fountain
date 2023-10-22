Design of Fountain
==================

Design Goals
------------

Fountain should:

*   Be a grammar formalism, as opposed to a programming language.
*   Permit implementations that perform both efficient parsing of
    strings, and efficient generation of strings, based on the
    same grammar description.
*   Be able to express any Context-Sensitive Language (CSL).
*   Be able to express _only_ the Context-Sensitive Languages (lofty goal; see below).
*   Allow these CSLs, and any supplementary information needed to
    support efficient parsing and generation from them, to be expressed
    as concisely and cleanly as possible.

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
CFLs end and the CSLs begin, less is known about exactly where the
CSLs end and the non-CSLs begin.

Here's what is known:

*   Parsing a CSL is a PSPACE-complete problem.  We know this because
    every CSL corresponds to a linear bounded automaton (LBA), and
    the problem of simulating an LBA (i.e. take a description of
    an LBA and a string on input, accept iff LBA accepts that string)
    is PSPACE-complete.
*   Computing a primitive recursive function is NEXPTIME-complete.  That
    is, the problem of taking a description of a PR function and an
    integer and accepting if that function computes that integer and
    rejecting if it doesn't, is in NEXPTIME, and is at least as complex
    as any other problem in NEXPTIME.
*   PSPACE is known to be contained in NEXPTIME, but the containment
    is not known to be strict.
*   Therefore we don't know if there are any problems that are in
    NEXPTIME that aren't in PSPACE.
*   Therefore we don't know if there are any PR functions which cannot
    be expressed as a CSL. (Because if there were, we could seperate
    NEXPTIME and PSPACE, and we haven't.)

Therefore we don't have an indication of which PR functions are
expressible as CSLs, and which aren't.

Therefore we don't have a good way to look at a Fountain grammar and
say if it describes a CSL, or something outside the CSLs (for example,
a primitive recursive function).

#### Is generating a string that is a member of a CSL also PSPACE-complete?

I haven't come across anything specifically about this in the literature,
but I believe it is, for the following reasons.

An acceptor for a CSL can be turned into a parser for that CSL, one which
outputs some of its context (e.g. outputting `n=3` when given `aaabbbccc`),
efficiently.  When looking at the CSL as an LBA, this is trivial: the
context will remain somewhere on the tape at the end, so just output that.

Now, this parser can also be viewed as a transducer, and we can think of
running it in reverse (e.g. it outputs `aaabbbccc` when given `n=3`;
this sort of thing is exactly Fountain's purpose).

In the worst case, running this reverse process means running the original
parser nondeterministically and accepting if there is an accepting path
from some input to our goal.  This implies we would need NPSPACE to run
this transducer in reverse.  But by Savitch's Theorem, PSPACE = NPSPACE,
so we could still do this on an LBA, so it would still be a CSL.

For how Fountain is currently defined, it could also be the case that
the generation goals are not sufficiently defined, and Fountain keeps
searching forever for a satisfying string, and never finds
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

#### What if we impose linear bounds on the storage that Fountain uses?

The fact that every CSL can be captured by an LBA suggests maybe we
could just linear-bound the amount of storage used by a Fountain
grammar in the worst case.

Having thought about it, this is probably the way to go.  When
processing (parsing or generating) a Fountain grammar, the user ought
to be able to specify a "fuel efficiency" _E_, which is the linear bound.

(Whether this is specified in the source file, or through some other
means like a command-line option, is immaterial for the present
purposes.  Presumbly though, it's omission doesn't stop us from
processing the grammar, we simply don't make the check in this case)

Each time a character is consumed from the input (resp. generated to
the output), _E_ units of "fuel" are gained.  Each time a new unit of
storage is allocated for storing the context used by the grammar,
one unit of "fuel" is expended.  Expending more fuel than has been
accumulated so far results in some kind of warning or error condition
(the salient thing being that the user is made aware that this grammar
exceeds the linear bound.)

It should be noted that the integers are unbounded, so an
operation like `a += 1`, may or may not allocate a new unit of
storage (a machine word, say), so the usage needs to be recalculated
afterwards.

Freeing up storage does not allow the grammar to reclaim "fuel".

This check could probably be done statically, using some kind of
abstract interpretation; but it would also be possible (and probably
a lot easier) to add it as a dynamic check while processing the
grammar.

#### Does all this talk of complexity classes even mean anything?

In the end it means very little.  The whole purpose of Fountain is
to permit the construction of grammars that can both parse and
generate _efficiently_.  So I think we can reasonably assume that
most of the time the grammar author will aim to construct something
that runs in polynomial time, even though they have all of PSPACE
at their disposal.  Restricting Fountain to express exactly the
CSLs is more of a theoretically satisfying goal than a practical one.

### Should we think of constraints as relational operators?

The design of Fountain came from a path of exploration that led
through relational programming.  I wanted a grammar formalism that
could be run both "forwards" and "backwards", and logic programming
languages such as Prolog support that (and relational programming
languages such as miniKanren support it even moreso.)  So when I
was putting together the basic parts of Fountain, my idea was that
a constraint like `<. a += 1 .>` would be something like
`A1 is A + 1` in Prolog; more specifically, it would relate the parse
state on the LHS of `<. a += 1 .>` to the parse state on the RHS of
it, specifying that on the RHS, the variable `a` is one more than it
is on the LHS.  The string of terms in the production could then be
just as easily interpreted right-to-left as left-to-right.

And I still think it is conceptually advantageous, and in some sense
proper, to think of these constraints this way.  However, it turns out
that it's not all that necessary to look at them this way when actually
implementing Fountain.  Whether parsing or generating, the terms
of a grammar production are processed left-to-right, and the variable
in a rule such as `<. a += 1 .>` really can be updated in place
(which is exactly what we want, to support efficient processing)
and so really is an updatable variable.

In the end, I probably could have come to a design very similar
to the present design of Fountain without travelling though the
territory of relational programming at all, simply by taking
[ambinate.py][] and defunctionalizing it.

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

### How can we apply randomness during generation?

When generating from a grammar, we often want to take a "random sample"
of the space of utterances that the grammar defines.  There are methods
that have been developed to do this; not just for grammars, but any
recursive description of a structure; for example [Boltzmann Samplers][]
(PDF).

We should probably go in this direction.

We can thread an explicit pseudo-random number generator through a
Fountain grammar currently, by passing a parameter up and down through
the productions, calling a certain production with this parameter to
advance the pseudorandom generation state, and selecting on this state
in every relevant alternation operation.

However, this is tedious.

It would be much more convenient, for the Fountain grammar author,
for the Fountain implementation to choose an alternate
(pseudo)randomly whenever there are more than one possible
alternatives available in an alternation operation.

It is also worth thinking about the fact that we must also be able
to parse what we've pseudo-randomly generated.  But we probably
don't want to insist that its random choices follows a particular
generation sequence.  We probably want to consider it acceptable
even if arbitrary choices were made, as long as they were otherwise
consistent.  This is another reason to avoid threading an explicit
PRNG through the grammar.

The technical solution for doing this efficiently seems to be to,
during generation, at each alteration, find the set of possible
alternatives (the ones where the guard-constraints evaluate to
true) and pick randomly from them, using an internal, seedable PRNG.

### How should parameters with different data types be handled?

Currently, all parameters are values of unbounded integer type.
This is theoretically sufficient for all context-sensitive parsing
and generation tasks, but in practice it is terribly inconvenient.

It would be desirable for parameters to take on other data types.
This would include simple data types such as strings, and compound
data types such as lists of values, sets of values, and dictionaries
mapping values to values.

This raises a number of design questions though:

*   When parameters are declared, do we also want to declare their types?
*   Might we want variables of "string produced by a certain production" type?

[Exanoke]: https://catseye.tc/node/Exanoke
[Tamsin]: https://catseye.tc/node/Tamsin
[Tandem]: https://catseye.tc/node/Tandem
[Boltzmann Samplers]: https://algo.inria.fr/flajolet/Publications/DuFlLoSc04.pdf
[ambinate.py]: https://codeberg.org/catseye/Dipple/src/branch/master/python/ambinate.py
