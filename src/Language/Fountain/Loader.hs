{-# LANGUAGE FlexibleContexts #-}
module Language.Fountain.Loader where

import Text.ParserCombinators.Parsec

import Language.Fountain.Grammar
import Language.Fountain.Constraint


fountain = do
    ps <- many prod
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
    return $ Seq es

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
    n <- intlit
    return $ Inc v n

dec = do
    v <- variable
    keyword "-="
    n <- intlit
    return $ Dec v n

gt = do
    v <- variable
    keyword ">"
    n <- intlit
    return $ GreaterThan v n

lt = do
    v <- variable
    keyword "<"
    n <- intlit
    return $ LessThan v n

variable = do
    s <- lowWord
    return $ Var s

terminal = do
    s <- quotedString
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
    spaces

capWord = do
    c <- upper
    s <- many (alphaNum)
    spaces
    return (c:s)

lowWord = do
    c <- lower
    s <- many (alphaNum)
    spaces
    return (c:s)

intlit = do
    c <- digit
    cs <- many digit
    num <- return (read (c:cs) :: Integer)
    spaces
    return num

quotedString = do
    c1 <- char '"'
    s <- many $ satisfy (\x -> x /= '"')
    c2 <- char '"'
    spaces
    return s

--
-- Driver
--

parseFountain :: String -> Either ParseError Grammar
parseFountain text = parse fountain "" text

parseConstConstraint :: String -> (Variable, Integer)
parseConstConstraint text = case parse unifyConst "" text of
    Right (UnifyConst v i) -> (v, i)
