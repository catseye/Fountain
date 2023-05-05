{-# LANGUAGE FlexibleContexts #-}
module Language.Fountain.Loader where

import Text.ParserCombinators.Parsec

import Language.Fountain.Grammar
import Language.Fountain.Constraint


-- Grammar ::= {Production}.
-- Production ::= NonTerminal "::=" {Expr0}.
-- Expr0 ::= Expr1 {"|" Expr1}.
-- Expr1 ::= Term {Term}.
-- Term  ::= "{" Expr0 "}"
--         | "(" Expr0 ")"
--         | "<." Constraint ".>"
--         | Terminal
--         | NonTerminal.
-- Constraint ::= Variable Constrainer.
-- Constrainer ::= "arb" Variable
--               | "=" (Variable | IntLit)
--               | "+=" IntLit
--               | "-=" IntLit
--               | ">" IntLit
--               | "<" IntLit.


fountain = do
    ps <- many prod
    return (Grammar ps)

prod = do
    s <- capWord
    let nt = NT s
    keyword "::="
    e <- expr0
    keyword ";"
    return (nt, e)

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

constrainer = (try arb) <|> (try unifyConst) <|> (try unifyVar) <|> (try inc) <|> (try dec) -- <|> (try gt) <|> (try lt)

arb = do
    keyword "arb"
    v <- variable
    return $ Arb v

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

variable = do
    s <- lowWord
    return $ Var s

terminal = do
    s <- quotedString
    return $ Term $ T (head s)

nonterminal = do
    s <- capWord
    return $ Term $ NT s

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
