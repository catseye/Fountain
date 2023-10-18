module Language.Fountain.Preprocessor where

import Data.List

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

coalesceConstraints (Seq exprs) = Seq (cc exprs)
coalesceConstraints (Alt exprs) = Alt (map coalesceConstraints exprs)
coalesceConstraints (Loop expr cs) = Loop (coalesceConstraints expr) cs  -- cs should be empty here actually, because decorateLoops comes later
coalesceConstraints other = other

cc :: [Expr] -> [Expr]
cc [] = []
cc (Constraint c1:Constraint c2:rest) =
    cc ((Constraint $ Both c1 c2):rest)
cc (other:rest) = other:(cc rest)

--
-- Copy any constraints that immediately follow a loop, into the loop itself.
--
decorateLoops :: Expr -> Expr
decorateLoops (Seq exprs) = Seq (preprocessSeq exprs) where
    preprocessSeq [] = []
    preprocessSeq ((Loop expr _):rest) =
        let
            expr' = decorateLoops expr
            (constraints, rest') = absorbConstraints rest
        in
            (Loop expr' constraints):(preprocessSeq rest')
    preprocessSeq (expr:rest) =
        (decorateLoops expr):(preprocessSeq rest)
    absorbConstraints :: [Expr] -> ([Constraint], [Expr])
    absorbConstraints exprs =
        let
            constraints' = map (extractConstraint) (takeWhile (isConstraint) exprs)
            exprs' = dropWhile (isConstraint) exprs

            isConstraint (Constraint _) = True
            isConstraint _ = False

            extractConstraint (Constraint c) = c
        in
            (constraints', exprs')
decorateLoops (Alt exprs) = Alt (map decorateLoops exprs)
decorateLoops (Loop expr _) = error "Cannot preprocess Loop that is not in Seq"
decorateLoops other = other

--
-- Any Alt with only a single child can be replaced by that single child.
--
eliminateSingleAlts (Alt [expr]) = eliminateSingleAlts expr
eliminateSingleAlts (Alt exprs) = Alt $ map (eliminateSingleAlts) exprs
eliminateSingleAlts (Seq exprs) = Seq $ map (eliminateSingleAlts) exprs
eliminateSingleAlts (Loop expr cs) = Loop (eliminateSingleAlts expr) cs
eliminateSingleAlts other = other
