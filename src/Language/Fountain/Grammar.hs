module Language.Fountain.Grammar
  (
    Expr(Seq, Alt, Loop, Terminal, NonTerminal, Constraint),
    Production(Production, ntname, params, backtrackable, constituents),
    Grammar(Grammar),
    depictExpr, depictExprs, depictProduction, depictGrammar, depictVars,
    startSymbol, production, getFormals,
  ) where

import Data.List (intercalate)

import Language.Fountain.Constraint


type NTName = String

data Expr = Seq [Expr]
          | Alt Bool [Expr]
          --
          -- In the case of a Loop, there is a post-processing step
          -- that copies any constraints following the Loop, into the Loop
          -- itself.  This is to make the generator's job easier.
          --
          | Loop Expr [Constraint]
          | Terminal Char
          | NonTerminal NTName [Variable]
          | Constraint Constraint
    deriving (Show, Ord, Eq)


data Production = Production {
    ntname :: String,
    params :: [Variable],
    backtrackable :: Bool,
    constituents :: Expr
} deriving (Show, Ord, Eq)


data Grammar = Grammar [Production]
    deriving (Show, Ord, Eq)

--
-- Pretty printing
--

depictExpr (Seq exprs) = "(" ++ (intercalate " " (map (depictExpr) exprs)) ++ ")"
depictExpr (Alt _ exprs) = "(" ++ (intercalate " | " (map (depictExpr) exprs)) ++ ")"
depictExpr (Loop expr _) = "{" ++ (depictExpr expr) ++ "}"
depictExpr (Terminal c) = "\"" ++ [c] ++ "\""
depictExpr (NonTerminal name vars) = name ++ depictVars vars
depictExpr (Constraint c) = "<. " ++ (depictConstraint c) ++ " .>"

depictExprs exprs = (intercalate ", " (map (depictExpr) exprs))

depictVars [] = ""
depictVars vars = "<" ++ (intercalate ", " (map (depictVar) vars)) ++ ">"

depictProduction p =
    (ntname p) ++ (depictVars $ params p) ++ (showBT $ backtrackable p) ++ " ::= " ++ (depictExpr (constituents p)) ++ ";\n"
    where showBT b = if b then "(*)" else ""

depictGrammar (Grammar []) = ""
depictGrammar (Grammar (prod:rest)) = (depictProduction prod) ++ (depictGrammar $ Grammar rest)

--
-- Accessors and utilities
--

startSymbol :: Grammar -> NTName
startSymbol (Grammar (prod : _)) = ntname prod
startSymbol (Grammar []) = error "No productions in grammar"

production :: NTName -> Grammar -> Expr
production nt (Grammar (prod : rest)) =
    if nt == (ntname prod) then constituents prod else production nt (Grammar rest)
production nt (Grammar []) = error ("Production '" ++ nt ++ "' not found")

getFormals :: NTName -> Grammar -> [Variable]
getFormals nt (Grammar (prod : rest)) =
    if nt == (ntname prod) then params prod else getFormals nt (Grammar rest)
getFormals nt (Grammar []) = error ("Production '" ++ nt ++ "' not found")

