module Language.Fountain.Preprocessor (
    preprocessGrammarForGeneration, preprocessGrammarForParsing
) where

import Language.Fountain.Grammar
import Language.Fountain.Constraint


preprocessGrammarForGeneration :: Grammar -> Grammar
preprocessGrammarForGeneration (Grammar productions) = Grammar $ map (preprocessProduction) productions where
    preprocessProduction p@Production{ constituents=c } = p { constituents=preprocessExpr c }
    preprocessExpr = eliminateSingleAlts . coalesceConstraints . decorateLoops


preprocessGrammarForParsing :: Grammar -> Grammar
preprocessGrammarForParsing (Grammar productions) = Grammar $ map (preprocessProduction) productions where
    preprocessProduction p@Production{ constituents=c } = p { constituents=preprocessExpr c }
    preprocessExpr = eliminateSingleAlts . coalesceConstraints


--
-- Coalesce constraints
--

coalesceConstraints (Seq exprs) = Seq (coalesceSeq exprs)
coalesceConstraints (Alt bt exprs) = Alt bt (map coalesceConstraints exprs)
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
            (constraints, _) = absorbConstraints rest
            -- Note that we do not move the constraints into the loop,
            -- we copy them.  When checking them when generating from
            -- the loop, we throw away that result so that we do not
            -- apply them twice.
        in
            (Loop expr' constraints):(decorateSeq rest)
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
decorateLoops (Alt bt exprs) = Alt bt (map decorateLoops exprs)
decorateLoops (Loop _expr _cs) = error "Cannot preprocess Loop that is not in Seq"
decorateLoops other = other

--
-- Any Alt with only a single child can be replaced by that single child.
--
eliminateSingleAlts (Alt _ [expr]) = eliminateSingleAlts expr
eliminateSingleAlts (Alt bt exprs) = Alt bt $ map (eliminateSingleAlts) exprs
eliminateSingleAlts (Seq exprs) = Seq $ map (eliminateSingleAlts) exprs
eliminateSingleAlts (Loop expr cs) = Loop (eliminateSingleAlts expr) cs
eliminateSingleAlts other = other
