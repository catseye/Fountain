Fountain Definition
===================

This document defines the Fountain Grammar Formalism.

It does this in part by test cases.  These test cases
are written in Falderal format.

Grammar of Fountain
-------------------

This grammar is written in EBNF.  Any amount of whitespace may occur
between tokens (and for this purpose, comments, which are introduced
by `//` and extend until the end of the line, count as whitespace).
Some whitespace must appear between tokens if the tokens would otherwise
be interpreted as a single token.  The bottommost productions in the
grammar describe the concrete structure of tokens; in these productions
no whitespace may appear between successive concrete terminals (the
symbols enclosed in big angle quotes.)  Note, this paragraph should
be rewritten for clarity at some point.

    Grammar ::= {Production}.
    Production ::= NonTerminal [Formals] ["(*)"] "::=" {Expr0}.
    Expr0 ::= Expr1 {"|" Expr1}.
    Expr1 ::= Term {Term}.
    Term  ::= "{" Expr0 "}"
            | "(" Expr0 ")"
            | "<." Constraint ".>"
            | Terminal
            | NonTerminal [Actuals].
    Formals ::= "<" Variable {"," Variable} ">".
    Actuals ::= "<" VarExpr {"," VarExpr} ">".
    VarExpr ::= Variable.  -- TODO: In future this might be richer.
    Constraint ::= Variable Constrainer.
    Constrainer ::= "=" (Variable | IntLit)
                  | "+=" CExpr
                  | "-=" CExpr
                  | ">" CExpr
                  | "<" CExpr.
    CExpr ::= Variable | IntLit.
    NonTerminal ::= <<upper>><<alphanumeric>>*.
    Terminal ::= <<">> <<any except ">>+ <<">> | <<#>>IntLit.
    IntLit ::= [<<->>] <<digit>>+.

Tests follow.

    -> Functionality "Parse using Fountain Grammar" is implemented by
    -> shell command "bin/fountain parse %(test-body-file) %(test-input-file)"

    -> Functionality "Parse using Fountain Grammar with fixed input parameter n=3" is implemented by
    -> shell command "bin/fountain parse %(test-body-file) %(test-input-file) n=3"

    -> Functionality "Generate using Fountain Grammar" is implemented by
    -> shell command "bin/fountain generate %(test-body-file)"

    -> Functionality "Generate using Fountain Grammar with input parameters" is implemented by
    -> shell command "bin/fountain generate %(test-body-file) %(test-input-text)"

Tests for Parsing
-----------------

    -> Tests for functionality "Parse using Fountain Grammar"

Sequence.

    Goal ::= "f" "o" "o";
    <=== foo
    ===> Success

    Goal ::= "f" #111 #111;
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

    Goal ::=
        <. a = 0 .> { "a" <. a += 1 .> } <. a = n .>
        <. b = 0 .> { "b" <. b += 1 .> } <. b = n .>
        <. c = 0 .> { "c" <. c += 1 .> } <. c = n .>
        ;
    <=== aaabbbccc
    ===> Success

This one fails at the `<. b = n .>` constraint.

    Goal ::=
        <. a = 0 .> { "a" <. a += 1 .> } <. a = n .>
        <. b = 0 .> { "b" <. b += 1 .> } <. b = n .>
        <. c = 0 .> { "c" <. c += 1 .> } <. c = n .>
        ;
    <=== aaabbccc
    ???> Failure

Integers used in constraints may be negative.

    Goal ::= <. a = -3 .> { "a" <. a += 1 .> } <. a = 0 .>;
    <=== aaa
    ===> Success

    Goal ::= <. a = -3 .> { "a" <. a += 1 .> } <. a = 0 .>;
    <=== aa
    ???> Failure

Increment and decrement constraints by constant.

    Goal ::= <. a = 3 .> "a" <. a += 3 .> "a" <. a -= 2 .> "a" <. a = 4 .>;
    <=== aaa
    ===> Success

Increment and decrement constraints by variable.

    Goal ::= <. a = 3 .> <. b = 4 .> <. c = 5 .> "a" <. a += b .> "a" <. a -= c .> "a" <. a = 2 .>;
    <=== aaa
    ===> Success

Greater-than and less-than constraints by constant.

    Goal ::= <. a = 3 .> <. a > 2 .> <. a < 4 .> "a";
    <=== a
    ===> Success

Greater-than and less-than constraints by variable.

    Goal ::= <. a = 3 .> <. h = 4 .> <. l = 2 .> <. a > l .> <. a < h .> "a";
    <=== a
    ===> Success

Greater-than-or-equal and less-than-or-equal constraints by constant.

    Goal ::= <. a = 3 .> <. a >= 2 .> <. a <= 4 .> "a";
    <=== a
    ===> Success

    Goal ::= <. a = 3 .> <. a >= 3 .> <. a <= 3 .> "a";
    <=== a
    ===> Success

Greater-than-or-equal and less-than-or-equal constraints by variable.

    Goal ::= <. a = 3 .> <. h = 4 .> <. l = 2 .> <. a >= l .> <. a <= h .> "a";
    <=== a
    ===> Success

    Goal ::= <. a = 3 .> <. h = 3 .> <. l = 3 .> <. a >= l .> <. a <= h .> "a";
    <=== a
    ===> Success

### Parsing with local variables

    Goal ::= "Hi" Sp "there" Sp "world" "!";
    Sp ::= <. n = 0 .> { " " <. n += 1 .> } <. n > 0 .>;
    <=== Hi there world!
    ===> Success

    Goal ::= "Hi" Sp "there" Sp "world" "!";
    Sp ::= <. n = 0 .> { " " <. n += 1 .> } <. n > 0 .>;
    <=== Hi     there  world!
    ===> Success

### Parsing with parameters

    Goal ::= "Hi" Sp<a> "there" Sp<a> "world" "!";
    Sp<x> ::= <. n = 0 .> { " " <. n += 1 .> } <. n > 0 .> <. n = x .>;
    <=== Hi there world!
    ===> Success

    Goal ::= "Hi" Sp<a> "there" Sp<a> "world" "!";
    Sp<x> ::= <. n = 0 .> { " " <. n += 1 .> } <. n > 0 .> <. n = x .>;
    <=== Hi   there   world!
    ===> Success

    Goal ::= "Hi" Sp<a> "there" Sp<a> "world" "!";
    Sp<n> ::= <. n = 0 .> { " " <. n += 1 .> } <. n > 0 .>;
    <=== Hi   there  world!
    ???> Failure

    Goal ::= "Hi" Sp<a> "there" Sp<b> "world" "!";
    Sp<n> ::= <. n = 0 .> { " " <. n += 1 .> } <. n > 0 .>;
    <=== Hi   there  world!
    ===> Success

### Parsing with external parameters

    -> Tests for functionality "Parse using Fountain Grammar with fixed input parameter n=3"

When parsing, parameters can also be supplied from external sources.

    Goal ::=
        <. a = 0 .> { "a" <. a += 1 .> } <. a = n .>
        <. b = 0 .> { "b" <. b += 1 .> } <. b = n .>
        <. c = 0 .> { "c" <. c += 1 .> } <. c = n .>
        ;
    <=== aaabbbccc
    ===> Success

    Goal ::=
        <. a = 0 .> { "a" <. a += 1 .> } <. a = n .>
        <. b = 0 .> { "b" <. b += 1 .> } <. b = n .>
        <. c = 0 .> { "c" <. c += 1 .> } <. c = n .>
        ;
    <=== aabbcc
    ???> Failure

### Backtracking

A production may be marked as allowing backtracking to
occur within it, with the `(*)` symbol.

9 is divisible by 3.

    Goal(*) ::= "a" { "bb" } "c" | "a" { "bbb" } "c";
    <=== abbbbbbbbbc
    ===> Success

10 is divisible by 2.

    Goal(*) ::= "a" { "bb" } "c" | "a" { "bbb" } "c";
    <=== abbbbbbbbbbc
    ===> Success

11 is not divisible by 2 or by 3.

    Goal(*) ::= "a" { "bb" } "c" | "a" { "bbb" } "c";
    <=== abbbbbbbbbbbc
    ???> Failure

Backtracking does not currently work as you would expect inside loops.

    Goal(*) ::= "a" { "bb" | "bbb" } "c";
    <=== abbbbbbbbbc
    ???> Failure

We can however write the loop as a recursive production.

    Goal(*) ::= "a" R;
    R(*)    ::= "bb" R | "bbb" R | "c";
    <=== abbbbbbbbbc
    ===> Success

But note, the "choice point scope" is limited to the alternation
expression.  So this formulation won't work:

    Goal(*) ::= "a" R "c";
    R(*)    ::= "bb" R | "bbb" R;
    <=== abbbbbbbbbc
    ???> Failure

Note how these don't work at all with backtracking disabled,
because two of the alternatives start with the same terminal.

    Goal ::= "a" { "bb" } "c" | "a" { "bbb" } "c";
    <=== abbbbbbbbbc
    ???> Multiple pre-conditions

Tests for Generation
--------------------

    -> Tests for functionality "Generate using Fountain Grammar"

Sequence.

    Goal ::= "f" "o" "o";
    ===> foo

Alternation.  Note that, when generating, Alt choices need preconditions because,
unlike parsing, we need some guidance of which one to pick.

    Goal ::= "f" | "o";
    ???> No pre-condition

    Goal ::= "f" | <. a = 0 .> "o";
    ???> No pre-condition

    Goal ::= (<. a = 0 .> "f") | "o";
    ???> No pre-condition

But if all choices of the Alt have constraints, we are able to select the one
that fulfills the constraints.

    Goal ::= <. a = 1 .> (<. a = 1 .> "f" | <. a = 0 .> "o");
    ===> f

    Goal ::= <. a = 0 .> (<. a = 1 .> "f" | <. a = 0 .> "o");
    ===> o

But only and exactly one of the choices must have its constraints satisfied by
the current state.  If more than one choice has satisfiable constraints, then
that is an ambiguous situation, and (in normal operation) it is an error.

    Goal ::= <. a = 0 .> "f" | <. a = 1 .> "o";
    ???> Multiple pre-conditions

    Goal ::= <. a = 0 .> (<. a = 0 .> "f" | <. a = 1 .> "o") (<. a = 1 .> "a" | <. a = 0 .> "z");
    ===> fz

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

This prior determination may happen outside of the processing of
the grammar proper.  The Fountain language does not prescribe
exactly how this must happen.  But it is expected that one way
is for these values to be provided as input, in much the same
manner the grammar itself is provided as input.

    -> Tests for functionality "Generate using Fountain Grammar with input parameters"

    Goal ::= <. a = 0 .> "a";
    <=== b=5
    ===> a

Thus we can show the language previously parsed can also be generated.

    Goal ::=
         <. a = 0 .> { "a" <. a += 1 .> } <. a = n .>
         <. b = 0 .> { "b" <. b += 1 .> } <. b = n .>
         <. c = 0 .> { "c" <. c += 1 .> } <. c = n .>
         ;
    <=== n=3
    ===> aaabbbccc

Increment and decrement constraints by constant.

    Goal ::= <. a = 3 .> "a" <. a += 3 .> "a" <. a -= 2 .> "a" <. a = 4 .>;
    ===> aaa

Increment and decrement constraints by variable.

    Goal ::= <. a = 3 .> <. b = 4 .> <. c = 5 .> "a" <. a += b .> "a" <. a -= c .> "a" <. a = 2 .>;
    ===> aaa

Greater-than and less-than constraints by constant.

    Goal ::= <. a = 3 .> <. a > 2 .> <. a < 4 .> "a";
    ===> a

Greater-than and less-than constraints by variable.

    Goal ::= <. a = 3 .> <. h = 4 .> <. l = 2 .> <. a > l .> <. a < h .> "a";
    ===> a

Greater-than-or-equal and less-than-or-equal constraints by constant.

    Goal ::= <. a = 3 .> <. a >= 2 .> <. a <= 4 .> "a";
    ===> a

    Goal ::= <. a = 3 .> <. a >= 3 .> <. a <= 3 .> "a";
    ===> a

Greater-than-or-equal and less-than-or-equal constraints by variable.

    Goal ::= <. a = 3 .> <. h = 4 .> <. l = 2 .> <. a >= l .> <. a <= h .> "a";
    ===> a

    Goal ::= <. a = 3 .> <. h = 3 .> <. l = 3 .> <. a >= l .> <. a <= h .> "a";
    ===> a

### Generation with local variables

    Goal ::= "Hi" Sp "there" Sp "world" "!";
    Sp ::= <. n = 0 .> { " " <. n += 1 .> } <. n > 0 .>;
    <=== 
    ===> Hi there world!

### Generation with external parameters

    Goal ::= "Hi" Sp<a> "there" Sp<a> "world" "!";
    Sp<x> ::= <. n = 0 .> { " " <. n += 1 .> } <. n > 0 .> <. n = x .>;
    <=== a=3
    ===> Hi   there   world!

### Backtracking

A production may be marked as allowing backtracking to
occur within it, with the `(*)` symbol.

Note however that the "choice point scope" for backtracking
is limited to the alternation expression.  So any failure
after (that is, outside of) the alternation expression won't
cause a backtrack to occur.

    Goal(*) ::= <. n = 0 .> ("a" | "b" <. n += 1 .>) ("a" <. n += 1 .> | "b") <. n = 2 .>;
    <=== 
    ???> Failure

So to get these to work, they need to be formulated in a
"tail recursive" way that may not be entirely natural.

    Goal(*)    ::= <. n = 0 .> One<n>;
    One<n>(*)  ::= ("a" Two<n> | "b" <. n += 1 .> Two<n>);
    Two<n>(*)  ::= ("a" <. n += 1 .> Three<n> | "b" Three<n>);
    Three<n>(*)::= <. n = 2 .>;
    <=== 
    ===> ba

The "tail recursive" production can be actually recursive
to allow unbounded extent on this.

Note that the alternation shown below is currently processed as
ordered choice.  This is not necessarily guaranteed.

    Goal<n>       ::= <. a = 0 .> Item<a, n>;
    Item<a, n>(*) ::= <. a = n .>
                    | "####" <. a += 4 .> <. a <= n .> Item<a, n>
                    | "ooooo" <. a += 5 .> <. a <= n .> Item<a, n>
                    | "xxxxxxx" <. a += 7 .> <. a <= n .> Item<a, n>;
    <=== n=30
    ===> ####################oooooooooo

You can't sum to 6 with these choices.

    Goal<n>       ::= <. a = 0 .> Item<a, n>;
    Item<a, n>(*) ::= <. a = n .>
                    | "####" <. a += 4 .> <. a <= n .> Item<a, n>
                    | "ooooo" <. a += 5 .> <. a <= n .> Item<a, n>
                    | "xxxxxxx" <. a += 7 .> <. a <= n .> Item<a, n>;
    <=== n=6
    ???> Failure

Note how these don't work at all with backtracking disabled,
because two of the alternatives start with the same terminal.

    Goal<n>       ::= <. a = 0 .> Item<a, n>;
    Item<a, n>    ::= <. a = n .>
                    | "####" <. a += 4 .> <. a <= n .> Item<a, n>
                    | "ooooo" <. a += 5 .> <. a <= n .> Item<a, n>
                    | "xxxxxxx" <. a += 7 .> <. a <= n .> Item<a, n>;
    <=== n=6
    ???> No pre-condition
