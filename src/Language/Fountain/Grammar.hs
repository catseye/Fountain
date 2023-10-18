module Language.Fountain.Grammar
  (
    Expr(Seq, Alt, Loop, Terminal, NonTerminal, Constraint), Grammar(Grammar),
    startSymbol, production, getFormals
  ) where

import Data.List (intercalate)

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
          | Terminal Char
          | NonTerminal NTName [Variable]
          | Constraint Constraint
    deriving (Ord, Eq)

instance Show Expr where
    show (Seq exprs) = "(" ++ (intercalate " " (map (show) exprs)) ++ ")"
    show (Alt exprs) = "(" ++ (intercalate " | " (map (show) exprs)) ++ ")"
    show (Loop expr _) = "{" ++ (show expr) ++ "}"
    show (Terminal c) = "\"" ++ [c] ++ "\""
    show (NonTerminal name vars) = name ++ showVars vars
    show (Constraint c) = "<. " ++ (show c) ++ " .>"


showVars [] = ""
showVars vars = "<" ++ (intercalate ", " (map (show) vars)) ++ ">"


data Grammar = Grammar [(NTName, [Variable], Expr)]
    deriving (Ord, Eq)

instance Show Grammar where
    show (Grammar []) = ""
    show (Grammar ((name,vars,expr):rest)) = name ++ (showVars vars) ++ " ::= " ++ (show expr) ++ ";\n" ++ (show $ Grammar rest)


startSymbol :: Grammar -> NTName
startSymbol (Grammar ((term, _, _) : _)) = term
startSymbol (Grammar []) = error "No productions in grammar"

production :: NTName -> Grammar -> Expr
production nt (Grammar ((term, _formals, expr) : rest)) =
    if term == nt then expr else production nt (Grammar rest)
production nt (Grammar []) = error ("Production '" ++ nt ++ "' not found")

getFormals :: NTName -> Grammar -> [Variable]
getFormals nt (Grammar ((term, formals, _expr) : rest)) =
    if term == nt then formals else getFormals nt (Grammar rest)
getFormals nt (Grammar []) = error ("Production '" ++ nt ++ "' not found")
