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
    ===> Grammar [(NT "Goal",Alt [Seq [Term (T 'f'),Term (T 'o'),Term (T 'o')]])]

Alternation and recursion.

    Goal ::= "(" Goal ")" | "0";
    ===> Grammar [(NT "Goal",Alt [Seq [Term (T '('),Term (NT "Goal"),Term (T ')')],Seq [Term (T '0')]])]

Repetition.

    Goal ::= "(" {"0"} ")";
    ===> Grammar [(NT "Goal",Alt [Seq [Term (T '('),Loop (Alt [Seq [Term (T '0')]]) [],Term (T ')')]])]

Constraints.

    Goal ::= <. arb n .>
         <. a = 0 .> { "a" <. a += 1 .> } <. a = n .>
         <. b = 0 .> { "b" <. b += 1 .> } <. b = n .>
         <. c = 0 .> { "c" <. c += 1 .> } <. c = n .>
         ;
    ===> Grammar [(NT "Goal",Alt [Seq [Constraint (Arb (Var "n")),Constraint (UnifyConst (Var "a") 0),Loop (Alt [Seq [Term (T 'a'),Constraint (Inc (Var "a") 1)]]) [],Constraint (UnifyVar (Var "a") (Var "n")),Constraint (UnifyConst (Var "b") 0),Loop (Alt [Seq [Term (T 'b'),Constraint (Inc (Var "b") 1)]]) [],Constraint (UnifyVar (Var "b") (Var "n")),Constraint (UnifyConst (Var "c") 0),Loop (Alt [Seq [Term (T 'c'),Constraint (Inc (Var "c") 1)]]) [],Constraint (UnifyVar (Var "c") (Var "n"))]])]

### Preprocessing

    -> Tests for functionality "Preprocess Fountain Grammar"

Sequence.

    Goal ::= "f" "o" "o";
    ===> Grammar [(NT "Goal",Alt [Seq [Term (T 'f'),Term (T 'o'),Term (T 'o')]])]

Alternation and recursion.

    Goal ::= "(" Goal ")" | "0";
    ===> Grammar [(NT "Goal",Alt [Seq [Term (T '('),Term (NT "Goal"),Term (T ')')],Seq [Term (T '0')]])]

Repetition.

    Goal ::= "(" {"0"} ")";
    ===> Grammar [(NT "Goal",Alt [Seq [Term (T '('),Loop (Alt [Seq [Term (T '0')]]) [],Term (T ')')]])]

    Goal ::= <. arb n .>
         <. a = 0 .> { "a" <. a += 1 .> } <. a = n .>
         <. b = 0 .> { "b" <. b += 1 .> } <. b = n .>
         <. c = 0 .> { "c" <. c += 1 .> } <. c = n .>
         ;
    ===> Grammar [(NT "Goal",Alt [Seq [Constraint (Arb (Var "n")),Constraint (UnifyConst (Var "a") 0),Loop (Alt [Seq [Term (T 'a'),Constraint (Inc (Var "a") 1)]]) [UnifyVar (Var "a") (Var "n"),UnifyConst (Var "b") 0],Loop (Alt [Seq [Term (T 'b'),Constraint (Inc (Var "b") 1)]]) [UnifyVar (Var "b") (Var "n"),UnifyConst (Var "c") 0],Loop (Alt [Seq [Term (T 'c'),Constraint (Inc (Var "c") 1)]]) [UnifyVar (Var "c") (Var "n")]]])]
