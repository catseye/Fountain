module Language.Fountain.Grammar where

import Language.Fountain.Constraint


type NTName = String

data Expr = Seq [Expr]
          | Alt [Expr]
          --
          -- In the case of a Loop, there is a post-processing step
          -- that copies any constraints following the Loop, into the Loop
          -- itself.  This is to make the generator's job easier.
          --
          | Loop Expr [Constraint]
          | Terminal String
          | NonTerminal NTName
          | Constraint Constraint
    deriving (Show, Ord, Eq)


data Grammar = Grammar [(NTName, Expr)]
    deriving (Show, Ord, Eq)

startSymbol (Grammar ((term, _) : _)) = term

production nt (Grammar ((term, expr) : rest)) =
    if term == nt then expr else production nt (Grammar rest)
