Fountain Definition
===================

This document defines the Fountain Grammar Formalism.

It does this in part by test cases.  These test cases
are written in Falderal format.

Grammar of Fountain
-------------------

This grammar is written in EBNF.  Any amount of whitespace may occur
between tokens (and for this purpose, comments count as whitespace).
Some whitespace must appear between tokens if the tokens would otherwise
be interpreted as a single token.  The bottommost productions in the
grammar describe the concrete structure of tokens.

    Grammar ::= {Production}.
    Production ::= NonTerminal "::=" {Expr0}.
    Expr0 ::= Expr1 {"|" Expr1}.
    Expr1 ::= Term {Term}.
    Term  ::= "{" Expr0 "}"
            | "(" Expr0 ")"
            | "<." Constraint ".>"
            | Terminal
            | NonTerminal.
    Constraint ::= Variable Constrainer.
    Constrainer ::= "arb" Variable
                  | "=" (Variable | IntLit)
                  | "+=" IntLit
                  | "-=" IntLit
                  | ">" IntLit
                  | "<" IntLit.
    NonTerminal ::= <<upper>><<alphanumeric>>*
    Terminal ::= <<">><<any>>+<<">>

The Tests
---------

    -> Functionality "Parse using Fountain Grammar" is implemented by
    -> shell command "bin/fountain parse %(test-body-file) %(test-input-file)"

    -> Functionality "Generate using Fountain Grammar" is implemented by
    -> shell command "bin/fountain generate %(test-body-file)"

    -> Functionality "Generate using Fountain Grammar with input parameters" is implemented by
    -> shell command "bin/fountain generate %(test-body-file) %(test-input-text)"

### Parsing

    -> Tests for functionality "Parse using Fountain Grammar"

Sequence.

    Goal ::= "f" "o" "o";
    <=== foo
    ===> Success

    Goal ::= "f" "o" "o";
    <=== fog
    ???> Failure

    Goal ::= "foo";
    <=== foom
    ===> Remaining: "m"

    Goal ::= "foo";
    <=== fo
    ???> Failure

Alternation and recursion.

    Goal ::= "(" Goal ")" | "0";
    <=== (((0)))
    ===> Success

    Goal ::= "(" Goal ")" | "0";
    <=== ()
    ???> Failure

    Goal ::= "(" Goal ")" | "0";
    <=== 0
    ===> Success

Repetition.

    Goal ::= "(" {"0"} ")";
    <=== (0)
    ===> Success

    Goal ::= "(" {"0"} ")";
    <=== (000000)
    ===> Success

    Goal ::= "(" {"0"} ")";
    <=== ()
    ===> Success

    Goal ::= "(" {"0"} ")";
    <=== (00001)
    ???> Failure

### Parsing with Constraints

This one succeeds because it satisfies all constraints.

    Goal ::= <. arb n .>
         <. a = 0 .> { "a" <. a += 1 .> } <. a = n .>
         <. b = 0 .> { "b" <. b += 1 .> } <. b = n .>
         <. c = 0 .> { "c" <. c += 1 .> } <. c = n .>
         ;
    <=== aaabbbccc
    ===> Success

This one fails at the `<. b = n .>` constraint.

    Goal ::= <. arb n .>
         <. a = 0 .> { "a" <. a += 1 .> } <. a = n .>
         <. b = 0 .> { "b" <. b += 1 .> } <. b = n .>
         <. c = 0 .> { "c" <. c += 1 .> } <. c = n .>
         ;
    <=== aaabbccc
    ???> Failure

### Generation

    -> Tests for functionality "Generate using Fountain Grammar"

Sequence.

    Goal ::= "f" "o" "o";
    ===> foo

Alternation.

    Goal ::= "f" | "o";
    ===> f

    Goal ::= ("f" | "o") ("a" | "z");
    ===> fa

Repetition.  Without constraints, this will error out.

    Goal ::= {"f"};
    ???> No postconditions defined for this Loop

### Generation with Constraints

Basic constraint checking during generation of a repeated section.

    Goal ::= <. a = 0 .> { "a" <. a += 1 .> } <. a = 5 .>;
    ===> aaaaa

Generation can also fail if constraints cannot be satisfied.

    Goal ::= <. a = 0 .> "a" <. a = 2 .>;
    ???> Failure

If an `arb` is encountered during generation, the value must have
already been determined.

    Goal ::= <. a = 0 .> "a" <. arb a .>;
    ===> a

    Goal ::= <. a = 0 .> "a" <. arb b .>;
    ???> Var "b" is unset

This prior determination may happen outside of the processing of
the grammar proper.  The Fountain language does not prescribe
exactly how this must happen.  But it is expected that one way
is for these values to be provided as input, in much the same
manner the grammar itself is provided as input.

    -> Tests for functionality "Generate using Fountain Grammar with input parameters"

    Goal ::= <. a = 0 .> "a" <. arb b .>;
    <=== b=5
    ===> a

Thus we can show the language previously parsed can also be generated.

    Goal ::= <. arb n .>
         <. a = 0 .> { "a" <. a += 1 .> } <. a = n .>
         <. b = 0 .> { "b" <. b += 1 .> } <. b = n .>
         <. c = 0 .> { "c" <. c += 1 .> } <. c = n .>
         ;
    <=== n=3
    ===> aaabbbccc
