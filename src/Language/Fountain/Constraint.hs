module Language.Fountain.Constraint where


data Variable = Var String
    deriving (Show, Ord, Eq)

data Constraint = UnifyConst Variable Integer
                | UnifyVar Variable Variable
                | Inc Variable Integer
                | Dec Variable Integer
                | GreaterThan Variable Integer
                | LessThan Variable Integer
    deriving (Show, Ord, Eq)
