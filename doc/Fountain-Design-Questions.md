Fountain Design Questions
=========================

### Can't a Definite Clause Grammar (DCG) do what Fountain does?

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

### Doesn't a Context-Sensitive Grammar (CSG) do what Fountain does?

It's true that every CSL can be described with a CSG -- in fact,
the CSLs are defined as the languages accepted by CSGs.  However, the
way that a CSG operates is very different from how Fountain operates.

By CSG I'm referring to "Type-1" in the Chomsky hierarchy.  The grammars
in the Chomsky hierarchy are basically string-rewriting systems.

When context is involved in such a rewriting system, that context is
"shuttled around" from production to production during parsing, much
like how the head of a single-tape Turing machine must traverse
potentially large distances of the tape, to access data that is
relevant to what it's doing at some other location on the tape.

For a theoretical construct this is fine.  For a practical parser
(or generator) though, all this shuttling makes it a really poor way
to parse (or generate) strings of a language.

Fountain, on the other hand, much like Attribute Grammars (AGs),
stores its context in an ancillary structure which is essentially
random-access and independent of the linear storage of the terminals
or nonterminals of the string being parsed or generated.  This makes
it a much more efficient choice for dealing with context-sensitivity.

### Isn't Fountain really a programming language in disguise?

Fountain resides in the murky twilight beween programming language and
grammar formalism, as all sufficiently powerful grammar formalisms must.

Using the grammar parts of Fountain feels like constructing a grammar
and defining a syntax; using the constraint parts of Fountain feels
like writing a program.  It's a design goal for it to feel as little
like writing a program as possible.  That is, it shouldn't be the case
that the user is required to do a lot of fiddly detail work, updating
variables and so forth, to achieve their desired effect.  Still, a
certain amount of that is inevitable, and I suspect it's not even well
understood how _much_ of that is inevitable.  It might be that a
great deal of fiddly work is required for most interesting tasks, and
that writing Fountain grammars unavoidably feels a lot like programming.
That would be a disappointing result.  The jury's still out though.

One property that we think of most programming languages having is that
they are Turing-complete.  One of Fountain's design goals is that it
not be Turing-complete; you cannot, e.g., write a Lisp interpreter in it.
This distances it from previous experiments by Cat's Eye Technologies
to design "grammar-like" programming languages, such as
[Tamsin][] and [Tandem][], which were intended to be Turing-complete.

### What complexity class does Fountain aim to capture?

Fountain exists for the purpose of to parsing and generating
context-sensitive languages.  To this end, it is a design goal
of Fountain grammars that they should be able to express
_all_ and _only_ the context-sensitive languages.

It turns out there are a number of snags with this goal, however, so we
treat it as more of a "moral" goal than a practically achievable one.

But let's talk about being restricted to CFLs first before exploring
those snags.

Parsing a context-sensitive language is a PSPACE-complete problem.
So if all a system can do is parse CSLs, it does mean that it isn't Turing-complete.
At the same time, PSPACE is a huge computational class, one which is
known to contain NP, and is thought to be quite a bit larger.
NP-complete problems already are generally considered "intractable";
so for all practical purposes, PSPACE ought to be ample.

### Is generating a string that is a member of a CSL also in PSPACE?

This is less clear.  I think there is a good chance it is, so long as
the generation goals (that is, the parameters given as input) are defined,
because then the grammar is something like a transducer: given a string,
get a set of parameters; given a set of parameters, get a string.  It
should be "equally context-sensitive" in both directions.

But I could be wrong.

It could also be the case that the generation goals are not sufficiently defined,
and Fountain keeps searching forever for a satisfying string, and never finds
one.

Then we'd need to weaken our complexity claim to something like "When generating
a string terminates, it uses only polynomial space".

### How does Fountain ensure no Fountain grammar strays outside PSPACE?

Preventing Fountain from expressing languages which are not
context-sensitive is still an open line of inquiry.  Clearly we
must disallow unrestricted recursion, but the best way of doing
this is less clear.

### What if we restrict Fountain to well-founded recursion?

Even if we limit the recursion to well-founded recursion (such
as found in [Exanoke][]), primitive recursive computations can
still be expressed.  Primitive recursion is known to be able to
solve any problem in NEXPTIME.

PSPACE is contained in NEXPTIME, but it's not known if the containment
is strict.  That is, it's not known if there are problems in NEXPTIME
that aren't in PSPACE.

Indeed there is something interesting here that I should write about.
If we could show that primitive recursion could accept some language
that a context-sensitive grammar could not accept, we could
seperate NEXPTIME and SPACE!  Given how many researchers have tried
to do that and not succeeded, it's not likely that me, in the course
of designing a computer language, will find a distinction that they
haven't.  Well-founded recursion would restrict us to NEXPTIME and
we don't _know_ that's not the same thing as being restricted to
PSPACE (i.e. the CFLs) -- so that is probably the best we can do
in practice.

### What if we forbid recursion entirely?

Forbidding recursion entirely will certainly prevent going outside
of NEXPTIME -- so long as we also confirm that every instance of
every repetition construct necessarily consumes one or more tokens
from the input string.  But expressing grammatical structures such
as nested parenthesis using repetition alone might be inconvenient
to the point of being obnoxious.  And we need to be careful to still
allow all CSLs to be expressed, which might prove cumbersome to show.

### How do we implement parameter passing?

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
