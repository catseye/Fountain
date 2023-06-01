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
    ===> Grammar [("Goal",[],Alt [Seq [Terminal 'f',Terminal 'o',Terminal 'o']])]

Multi-character terminals.

    Goal ::= "foo" "bar";
    ===> Grammar [("Goal",[],Alt [Seq [Seq [Terminal 'f',Terminal 'o',Terminal 'o'],Seq [Terminal 'b',Terminal 'a',Terminal 'r']]])]

Alternation and recursion.

    Goal ::= "(" Goal ")" | "0";
    ===> Grammar [("Goal",[],Alt [Seq [Terminal '(',NonTerminal "Goal" [],Terminal ')'],Seq [Terminal '0']])]

Repetition.

    Goal ::= "(" {"0"} ")";
    ===> Grammar [("Goal",[],Alt [Seq [Terminal '(',Loop (Alt [Seq [Terminal '0']]) [],Terminal ')']])]

Constraints.

    Goal ::=
         <. a = 0 .> { "a" <. a += 1 .> } <. a = n .>
         <. b = 0 .> { "b" <. b += 1 .> } <. b = n .>
         <. c = 0 .> { "c" <. c += 1 .> } <. c = n .>
         ;
    ===> Grammar [("Goal",[],Alt [Seq [Constraint (UnifyConst (Var "a") 0),Loop (Alt [Seq [Terminal 'a',Constraint (Inc (Var "a") (CInt 1))]]) [],Constraint (UnifyVar (Var "a") (Var "n")),Constraint (UnifyConst (Var "b") 0),Loop (Alt [Seq [Terminal 'b',Constraint (Inc (Var "b") (CInt 1))]]) [],Constraint (UnifyVar (Var "b") (Var "n")),Constraint (UnifyConst (Var "c") 0),Loop (Alt [Seq [Terminal 'c',Constraint (Inc (Var "c") (CInt 1))]]) [],Constraint (UnifyVar (Var "c") (Var "n"))]])]

Parameters and multiple productions.

    Goal ::= "Hi" Sp<a> "there" Sp<b> "world" "!";
    Sp<n> ::= <. n = 0 .> { " " <. n += 1 .> } <. n > 0 .>;
    ===> Grammar [("Goal",[],Alt [Seq [Seq [Terminal 'H',Terminal 'i'],NonTerminal "Sp" [Var "a"],Seq [Terminal 't',Terminal 'h',Terminal 'e',Terminal 'r',Terminal 'e'],NonTerminal "Sp" [Var "b"],Seq [Terminal 'w',Terminal 'o',Terminal 'r',Terminal 'l',Terminal 'd'],Terminal '!']]),("Sp",[Var "n"],Alt [Seq [Constraint (UnifyConst (Var "n") 0),Loop (Alt [Seq [Terminal ' ',Constraint (Inc (Var "n") (CInt 1))]]) [],Constraint (GreaterThan (Var "n") 0)]])]

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
    ===> Grammar [("Goal",[],Alt [Seq [Seq [Terminal 'f',Terminal 'o',Terminal 'o']]]),("A",[],Alt [Seq [Terminal 'f',Terminal 'o',Seq [Terminal '/',Terminal '/']]])]

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
    ===> Grammar [("Goal",[],Alt [Seq [Terminal 'f',Terminal 'o',Terminal 'o']])]

Alternation and recursion.

    Goal ::= "(" Goal ")" | "0";
    ===> Grammar [("Goal",[],Alt [Seq [Terminal '(',NonTerminal "Goal" [],Terminal ')'],Seq [Terminal '0']])]

Repetition.

    Goal ::= "(" {"0"} ")";
    ===> Grammar [("Goal",[],Alt [Seq [Terminal '(',Loop (Alt [Seq [Terminal '0']]) [],Terminal ')']])]

    Goal ::=
         <. a = 0 .> { "a" <. a += 1 .> } <. a = n .>
         <. b = 0 .> { "b" <. b += 1 .> } <. b = n .>
         <. c = 0 .> { "c" <. c += 1 .> } <. c = n .>
         ;
    ===> Grammar [("Goal",[],Alt [Seq [Constraint (UnifyConst (Var "a") 0),Loop (Alt [Seq [Terminal 'a',Constraint (Inc (Var "a") (CInt 1))]]) [UnifyVar (Var "a") (Var "n"),UnifyConst (Var "b") 0],Loop (Alt [Seq [Terminal 'b',Constraint (Inc (Var "b") (CInt 1))]]) [UnifyVar (Var "b") (Var "n"),UnifyConst (Var "c") 0],Loop (Alt [Seq [Terminal 'c',Constraint (Inc (Var "c") (CInt 1))]]) [UnifyVar (Var "c") (Var "n")]]])]
