module Language.Fountain.Constraint where


data Variable = Var String
    deriving (Show, Ord, Eq)

data Constraint = UnifyConst Variable Integer
                | UnifyVar Variable Variable
                | Arb Variable
                | Inc Variable Integer
                | Dec Variable Integer
                | GT Variable Integer
                | LT Variable Integer
    deriving (Show, Ord, Eq)
