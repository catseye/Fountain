module Language.Fountain.Preprocessor where

import Language.Fountain.Grammar
import Language.Fountain.Constraint


preprocessGrammar :: Grammar -> Grammar
preprocessGrammar (Grammar productions) =
    let
        productions' = map (\(term, formals, expr) -> (term, formals, preprocessExpr expr)) productions
    in
        Grammar productions'

preprocessExpr = eliminateSingleAlts . coalesceConstraints . decorateLoops


--
-- Coalesce constraints
--

coalesceConstraints (Seq exprs) = Seq (coalesceSeq exprs)
coalesceConstraints (Alt exprs) = Alt (map coalesceConstraints exprs)
coalesceConstraints (Loop expr cs) = Loop (coalesceConstraints expr) cs  -- cs should be empty here actually, because decorateLoops comes later
coalesceConstraints other = other

coalesceSeq :: [Expr] -> [Expr]
coalesceSeq [] = []
coalesceSeq (Constraint c1:Constraint c2:rest) =
    coalesceSeq ((Constraint $ Both c1 c2):rest)
coalesceSeq (other:rest) = other:(coalesceSeq rest)

--
-- Copy any constraints that immediately follow a loop, into the loop itself.
--
decorateLoops :: Expr -> Expr
decorateLoops (Seq seqExprs) = Seq (decorateSeq seqExprs) where
    decorateSeq [] = []
    decorateSeq ((Loop expr _):rest) =
        let
            expr' = decorateLoops expr
            (constraints, rest') = absorbConstraints rest
        in
            (Loop expr' constraints):(decorateSeq rest')
    decorateSeq (expr:rest) =
        (decorateLoops expr):(decorateSeq rest)
    absorbConstraints :: [Expr] -> ([Constraint], [Expr])
    absorbConstraints exprs =
        let
            constraints' = map (extractConstraint) (takeWhile (isConstraint) exprs)
            exprs' = dropWhile (isConstraint) exprs

            isConstraint (Constraint _) = True
            isConstraint _ = False

            extractConstraint (Constraint c) = c
            extractConstraint _ = error "not a constraint"
        in
            (constraints', exprs')
decorateLoops (Alt exprs) = Alt (map decorateLoops exprs)
decorateLoops (Loop _expr _cs) = error "Cannot preprocess Loop that is not in Seq"
decorateLoops other = other

--
-- Any Alt with only a single child can be replaced by that single child.
--
eliminateSingleAlts (Alt [expr]) = eliminateSingleAlts expr
eliminateSingleAlts (Alt exprs) = Alt $ map (eliminateSingleAlts) exprs
eliminateSingleAlts (Seq exprs) = Seq $ map (eliminateSingleAlts) exprs
eliminateSingleAlts (Loop expr cs) = Loop (eliminateSingleAlts expr) cs
eliminateSingleAlts other = other
