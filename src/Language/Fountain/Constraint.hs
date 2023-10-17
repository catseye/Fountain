module Language.Fountain.Constraint where


data Variable = Var String
    deriving (Ord, Eq)

instance Show Variable where
    show (Var s) = s

-- A "c-expr" is a simple expression appearing inside a constraint.
data CExpr = CVar Variable
           | CInt Integer
    deriving (Ord, Eq)

instance Show CExpr where
    show (CVar v) = show v
    show (CInt i) = show i

data Constraint = UnifyConst Variable Integer
                | UnifyVar Variable Variable
                | Inc Variable CExpr
                | Dec Variable CExpr
                | GreaterThan Variable CExpr
                | GreaterThanOrEqual Variable CExpr
                | LessThan Variable CExpr
                | LessThanOrEqual Variable CExpr
                | Both Constraint Constraint
    deriving (Ord, Eq)

instance Show Constraint where
    show (UnifyConst v i) = (show v) ++ " = " ++ (show i)
    show (UnifyVar v w) = (show v) ++ " = " ++ (show w)
    show (Inc v e) = (show v) ++ " += " ++ (show e)
    show (Dec v e) = (show v) ++ " -= " ++ (show e)
    show (GreaterThan v e) = (show v) ++ " > " ++ (show e)
    show (GreaterThanOrEqual v e) = (show v) ++ " >= " ++ (show e)
    show (LessThan v e) = (show v) ++ " < " ++ (show e)
    show (LessThanOrEqual v e) = (show v) ++ " <= " ++ (show e)
    show (Both c1 c2) = "&& " ++ (show c1) ++ ", " ++ (show c2)
