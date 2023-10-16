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

preprocessExpr :: Expr -> Expr
preprocessExpr (Seq exprs) = Seq (preprocessSeq exprs) where
    preprocessSeq [] = []
    preprocessSeq ((Loop expr _):rest) =
        let
            expr' = preprocessExpr expr
            (constraints, rest') = absorbConstraints rest
        in
            (Loop expr' constraints):(preprocessSeq rest')
    preprocessSeq (expr:rest) =
        (preprocessExpr expr):(preprocessSeq rest)
preprocessExpr (Alt [expr]) = Seq [preprocessExpr expr]  -- FIXME: flatten Alts in a more thorough way
preprocessExpr (Alt exprs) = Alt (map preprocessExpr exprs)
preprocessExpr (Loop expr _) = error "Cannot preprocess Loop that is not in Seq"
preprocessExpr other = other


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
