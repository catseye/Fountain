{-# LANGUAGE FlexibleContexts #-}
module Language.Fountain.Loader where

import Data.Char (chr)
import Text.ParserCombinators.Parsec

import Language.Fountain.Grammar
import Language.Fountain.Constraint


fountain = do
    fspaces
    ps <- many prod
    fspaces
    eof
    return (Grammar ps)

prod = do
    nt <- capWord
    p <- option [] formals
    keyword "::="
    e <- expr0
    keyword ";"
    return (nt, p, e)

formals = do
    keyword "<"
    v <- sepBy (variable) (keyword ",")
    keyword ">"
    return v

expr0 = do
    es <- sepBy (expr1) (keyword "|")
    return $ Alt es

expr1 = do
    es <- many1 term
    return $ Seq $ flattenseq es

flattenseq [] = []
flattenseq (s:ss) = case s of
    -- Note that xs will always be flat already
    Seq xs -> xs ++ (flattenseq ss)
    _      -> (s:flattenseq ss)

term = (try parenExpr) <|> (try loopExpr) <|> (try constraintExpr) <|> (try terminal) <|> nonterminal

parenExpr = do
    keyword "("
    e <- expr0
    keyword ")"
    return e

loopExpr = do
    keyword "{"
    e <- expr0
    keyword "}"
    return $ Loop e []

constraintExpr = do
    keyword "<."
    c <- constrainer
    keyword ".>"
    return $ Constraint $ c

constrainer = (try unifyConst) <|> (try unifyVar) <|> (try inc) <|> (try dec) <|> (try gt) <|> (try lt)

unifyConst = do
    v <- variable
    keyword "="
    n <- intlit
    return $ UnifyConst v n

unifyVar = do
    v <- variable
    keyword "="
    w <- variable
    return $ UnifyVar v w

inc = do
    v <- variable
    keyword "+="
    e <- cexpr
    return $ Inc v e

dec = do
    v <- variable
    keyword "-="
    e <- cexpr
    return $ Dec v e

gt = do
    v <- variable
    keyword ">"
    e <- cexpr
    return $ GreaterThan v e

lt = do
    v <- variable
    keyword "<"
    e <- cexpr
    return $ LessThan v e

variable = do
    s <- lowWord
    return $ Var s

cexpr = (try cIntExpr) <|> cVarExpr

cIntExpr = do
    i <- intlit
    return $ CInt i

cVarExpr = do
    v <- variable
    return $ CVar v

terminal = do
    s <- quotedString <|> charlit
    case s of
        [c] -> return $ Terminal $ c
        _ -> return $ Seq $ map (\c -> Terminal c) s

nonterminal = do
    s <- capWord
    a <- option [] actuals
    return $ NonTerminal s a

actuals = do
    keyword "<"
    v <- sepBy (variable) (keyword ",")
    keyword ">"
    return v

--
-- Low level: Concrete things
--

keyword s = do
    try (string s)
    fspaces

capWord = do
    c <- upper
    s <- many (alphaNum)
    fspaces
    return (c:s)

lowWord = do
    c <- lower
    s <- many (alphaNum)
    fspaces
    return (c:s)

intlit = do
    sign <- option 1 leadingMinus
    c <- digit
    cs <- many digit
    fspaces
    return ((read (c:cs) :: Integer) * sign)

leadingMinus :: Parser Integer
leadingMinus = do
    char '-'
    return (-1)

quotedString = do
    char '"'
    s <- many $ satisfy (\x -> x /= '"')
    char '"'
    fspaces
    return s

charlit = do
    char '#'
    c <- digit
    cs <- many digit
    fspaces
    return [chr (read (c:cs) :: Int)]

fspaces = do
    spaces
    many comment
    return ()

comment = do
    string "//"
    many $ satisfy (\x -> x /= '\n')
    (do { char '\n'; return ()} <|> eof)
    fspaces
    return ()

--
-- Driver
--

parseFountain :: String -> Either ParseError Grammar
parseFountain text = parse fountain "" text

parseConstConstraint :: String -> (Variable, Integer)
parseConstConstraint text = case parse unifyConst "" text of
    Right (UnifyConst v i) -> (v, i)
