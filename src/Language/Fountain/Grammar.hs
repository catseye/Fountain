module Language.Fountain.Grammar
  (
    Expr(Seq, Alt, Loop, Terminal, NonTerminal, Constraint),
    Production(Production, ntname, params, backtrackable, constituents),
    Grammar(Grammar),
    depictExpr, depictProduction, depictGrammar, depictVars,
    startSymbol, production, getFormals,
    getPreCondition, missingPreConditions
  ) where

import Data.List (intercalate)
import Data.Maybe (mapMaybe)

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

depictVars [] = ""
depictVars vars = "<" ++ (intercalate ", " (map (show) vars)) ++ ">"

depictProduction p =
    (ntname p) ++ (depictVars $ params p) ++ (showBT $ backtrackable p) ++ " ::= " ++ (show (constituents p)) ++ ";\n"
    where showBT b = if b then "(*)" else ""

depictGrammar (Grammar []) = ""
depictGrammar (Grammar (prod:rest)) = (show prod) ++ (show $ Grammar rest)

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

--
-- Alt choices need preconditions because, especially in generating,
-- we need some guidance of which one to pick.
--
-- In parsing too though, it helps for being efficient and not
-- backtracking unnecessarily.  (But we need a more refined notion
-- in this case, because a terminal counts as a precondition.  TODO.)
--
getPreCondition :: Expr -> Maybe Constraint
getPreCondition (Seq (x:_)) = getPreCondition x
getPreCondition (Constraint c) = Just c
getPreCondition _ = Nothing

missingPreConditions choices =
    mapMaybe (\x -> case getPreCondition x of
        Just _ -> Nothing
        Nothing -> Just x
      ) choices
