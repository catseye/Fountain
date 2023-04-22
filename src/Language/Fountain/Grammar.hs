module Language.Fountain.Grammar where

import Language.Fountain.Constraint


data Term = T Char
          | NT String
    deriving (Show, Ord, Eq)

data Expr = Seq [Expr]
          | Alt [Expr]
          | Loop Expr [Constraint]  -- see below
          | Term Term
          | Constraint Constraint
    deriving (Show, Ord, Eq)

data Grammar = Grammar [(Term, Expr)]
    deriving (Show, Ord, Eq)


startSymbol (Grammar ((term, _) : _)) = term

production nt (Grammar ((term, expr) : rest)) =
    if term == nt then expr else production nt (Grammar rest)


-- In the case of a Loop, there will be a post-processing step
-- that copies any constraints following the Loop, into the Loop
-- itself.  This is to make the generator's life easier.
