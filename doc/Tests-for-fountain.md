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

Multi-character terminal.

    Goal ::= "foo";
    ===> Grammar [("Goal",[],Alt [Seq [Seq [Terminal 'f',Terminal 'o',Terminal 'o']]])]

Alternation and recursion.

    Goal ::= "(" Goal ")" | "0";
    ===> Grammar [("Goal",[],Alt [Seq [Terminal '(',NonTerminal "Goal",Terminal ')'],Seq [Terminal '0']])]

Repetition.

    Goal ::= "(" {"0"} ")";
    ===> Grammar [("Goal",[],Alt [Seq [Terminal '(',Loop (Alt [Seq [Terminal '0']]) [],Terminal ')']])]

Constraints.

    Goal ::= <. arb n .>
         <. a = 0 .> { "a" <. a += 1 .> } <. a = n .>
         <. b = 0 .> { "b" <. b += 1 .> } <. b = n .>
         <. c = 0 .> { "c" <. c += 1 .> } <. c = n .>
         ;
    ===> Grammar [("Goal",[],Alt [Seq [Constraint (Arb (Var "n")),Constraint (UnifyConst (Var "a") 0),Loop (Alt [Seq [Terminal 'a',Constraint (Inc (Var "a") 1)]]) [],Constraint (UnifyVar (Var "a") (Var "n")),Constraint (UnifyConst (Var "b") 0),Loop (Alt [Seq [Terminal 'b',Constraint (Inc (Var "b") 1)]]) [],Constraint (UnifyVar (Var "b") (Var "n")),Constraint (UnifyConst (Var "c") 0),Loop (Alt [Seq [Terminal 'c',Constraint (Inc (Var "c") 1)]]) [],Constraint (UnifyVar (Var "c") (Var "n"))]])]

### Preprocessing

    -> Tests for functionality "Preprocess Fountain Grammar"

Sequence.

    Goal ::= "f" "o" "o";
    ===> Grammar [("Goal",[],Alt [Seq [Terminal 'f',Terminal 'o',Terminal 'o']])]

Alternation and recursion.

    Goal ::= "(" Goal ")" | "0";
    ===> Grammar [("Goal",[],Alt [Seq [Terminal '(',NonTerminal "Goal",Terminal ')'],Seq [Terminal '0']])]

Repetition.

    Goal ::= "(" {"0"} ")";
    ===> Grammar [("Goal",[],Alt [Seq [Terminal '(',Loop (Alt [Seq [Terminal '0']]) [],Terminal ')']])]

    Goal ::= <. arb n .>
         <. a = 0 .> { "a" <. a += 1 .> } <. a = n .>
         <. b = 0 .> { "b" <. b += 1 .> } <. b = n .>
         <. c = 0 .> { "c" <. c += 1 .> } <. c = n .>
         ;
    ===> Grammar [("Goal",[],Alt [Seq [Constraint (Arb (Var "n")),Constraint (UnifyConst (Var "a") 0),Loop (Alt [Seq [Terminal 'a',Constraint (Inc (Var "a") 1)]]) [UnifyVar (Var "a") (Var "n"),UnifyConst (Var "b") 0],Loop (Alt [Seq [Terminal 'b',Constraint (Inc (Var "b") 1)]]) [UnifyVar (Var "b") (Var "n"),UnifyConst (Var "c") 0],Loop (Alt [Seq [Terminal 'c',Constraint (Inc (Var "c") 1)]]) [UnifyVar (Var "c") (Var "n")]]])]
