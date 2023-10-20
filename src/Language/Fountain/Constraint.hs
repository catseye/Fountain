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
                | GreaterThanOrEqual Variable CExpr
                | LessThan Variable CExpr
                | LessThanOrEqual Variable CExpr
                | Both Constraint Constraint
                | Lookahead String
    deriving (Show, Ord, Eq)

depictVar (Var s) = s

depictCExpr (CVar v) = depictVar v
depictCExpr (CInt i) = show i

depictConstraint (UnifyConst v i) = (depictVar v) ++ " = " ++ (show i)
depictConstraint (UnifyVar v w) = (depictVar v) ++ " = " ++ (depictVar w)
depictConstraint (Inc v e) = (depictVar v) ++ " += " ++ (depictCExpr e)
depictConstraint (Dec v e) = (depictVar v) ++ " -= " ++ (depictCExpr e)
depictConstraint (GreaterThan v e) = (depictVar v) ++ " > " ++ (depictCExpr e)
depictConstraint (GreaterThanOrEqual v e) = (depictVar v) ++ " >= " ++ (depictCExpr e)
depictConstraint (LessThan v e) = (depictVar v) ++ " < " ++ (depictCExpr e)
depictConstraint (LessThanOrEqual v e) = (depictVar v) ++ " <= " ++ (depictCExpr e)
depictConstraint (Both c1 c2) = "&& " ++ (depictConstraint c1) ++ ", " ++ (depictConstraint c2)
depictConstraint (Lookahead s) = "token is " ++ (show s)
