Tests for `fountain`
====================

These tests are specific to the reference implementation
of Fountain, `fountain`, and make reference to implementation
details.  They are thus not part of the main test suite.

    -> Functionality "Load Fountain Grammar" is implemented by
    -> shell command "bin/fountain load %(test-body-file)"

    -> Functionality "Preprocess Fountain Grammar" is implemented by
    -> shell command "bin/fountain preprocess %(test-body-file)"

### Loading

    -> Tests for functionality "Load Fountain Grammar"

Sequence.

    Goal ::= "f" "o" "o";
    ===> Goal ::= (("f" "o" "o"));

Multi-character terminals.

    Goal ::= "foo" "bar";
    ===> Goal ::= (("f" "o" "o" "b" "a" "r"));

Alternation and recursion.

    Goal ::= "(" Goal ")" | "0";
    ===> Goal ::= (("(" Goal ")") | ("0"));

Repetition.

    Goal ::= "(" {"0"} ")";
    ===> Goal ::= (("(" {(("0"))} ")"));

Constraints.

    Goal ::=
         <. a = 0 .> { "a" <. a += 1 .> } <. a = n .>
         <. b = 0 .> { "b" <. b += 1 .> } <. b = n .>
         <. c = 0 .> { "c" <. c += 1 .> } <. c = n .>
         ;
    ===> Goal ::= ((<. a = 0 .> {(("a" <. a += 1 .>))} <. a = n .> <. b = 0 .> {(("b" <. b += 1 .>))} <. b = n .> <. c = 0 .> {(("c" <. c += 1 .>))} <. c = n .>));

Parameters and multiple productions.

    Goal ::= "Hi" Sp<a> "there" Sp<b> "world" "!";
    Sp<n> ::= <. n = 0 .> { " " <. n += 1 .> } <. n > 0 .>;
    ===> Goal ::= (("H" "i" Sp<a> "t" "h" "e" "r" "e" Sp<b> "w" "o" "r" "l" "d" "!"));
    ===> Sp<n> ::= ((<. n = 0 .> {((" " <. n += 1 .>))} <. n > 0 .>));

Comments.

    // This is my grammar.
    // A comment may come before it.
    
    Goal ::= "foo";  // You see
    // how it is
    A ::= "f"  // You see
    "o" // how
    //
    // There are many ways to place comments.
    "//";
    ===> Goal ::= (("f" "o" "o"));
    ===> A ::= (("f" "o" "/" "/"));

Misplaced semicolon is a syntax error.

    Goal ::= <. x = 0 .> <. a = 0 .> { DogMove<x, d> <. a += 1 .> } <. a = 1000 .>;
    DogMove<x, d> ::= <. d > 0 .> "The dog ran away." #10; <. x += 1 .>
                    | <. d < 0 .> "The dog came back." #10; <. x -= 1 .>
                    ;
    ???> unexpected '<'

### Preprocessing

    -> Tests for functionality "Preprocess Fountain Grammar"

Sequence.

    Goal ::= "f" "o" "o";
    ===> Goal ::= ("f" "o" "o");

Alternation and recursion.

    Goal ::= "(" Goal ")" | "0";
    ===> Goal ::= (("(" Goal ")") | ("0"));

Repetition.

    Goal ::= "(" {"0"} ")";
    ===> Goal ::= ("(" {("0")} ")");

    Goal ::=
         <. a = 0 .> { "a" <. a += 1 .> } <. a = n .>
         <. b = 0 .> { "b" <. b += 1 .> } <. b = n .>
         <. c = 0 .> { "c" <. c += 1 .> } <. c = n .>
         ;
    ===> Goal ::= (<. a = 0 .> {("a" <. a += 1 .>)} {("b" <. b += 1 .>)} {("c" <. c += 1 .>)});
