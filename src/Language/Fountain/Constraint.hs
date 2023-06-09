module Language.Fountain.Constraint where


data Variable = Var String
    deriving (Show, Ord, Eq)

-- A "c-expr" is a simple expression appearing inside a constraint.
data CExpr = CVar Variable
           | CInt Integer
    deriving (Show, Ord, Eq)

data Constraint = UnifyConst Variable Integer
                | UnifyVar Variable Variable
                | Inc Variable CExpr
                | Dec Variable CExpr
                | GreaterThan Variable CExpr
                | LessThan Variable CExpr
    deriving (Show, Ord, Eq)
