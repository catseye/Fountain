module Language.Fountain.Parser (parseFrom, obtainResult) where

import Language.Fountain.Grammar
import Language.Fountain.Constraint
import Language.Fountain.Store


data ParseState = Parsing String Store
                | Failure
    deriving (Show, Ord, Eq)


expectTerminal :: Char -> ParseState -> ParseState
expectTerminal tc (Parsing (c:cs) a) = if c == tc then (Parsing cs a) else Failure
expectTerminal tc (Parsing [] a) = Failure
expectTerminal tc Failure = Failure

obtainResult :: ParseState -> Either String String
obtainResult (Parsing s _) = Right s
obtainResult Failure = Left "failure"

updateStore [] [] sourceStore destStore = destStore
updateStore (sourceKey:sourceKeys) (destKey:destKeys) sourceStore destStore =
    -- Populate destKey in the new store with the value at sourceKey in the sourceStore
    let
        destStore' = case fetch sourceKey sourceStore of
            Just val -> insert destKey val destStore
            Nothing -> destStore
    in
        updateStore sourceKeys destKeys sourceStore destStore'


parse :: Grammar -> ParseState -> Expr -> ParseState

parse g st (Seq s) = parseSeq g st s where
    parseSeq g st [] = st
    parseSeq g st (e : rest) =
        case parse g st e of
            Failure -> Failure
            st'     -> parseSeq g st' rest

parse g st (Alt s) = parseAlt g st s where
    parseAlt g st [] = Failure
    parseAlt g st (e : rest) =
        case parse g st e of
            Failure -> parseAlt g st rest
            st'     -> st'

parse g st (Loop l _) = parseLoop g st l where
    parseLoop g st e =
        case parse g st e of
            Failure -> st
            st'     -> parseLoop g st' e

parse g st (Terminal c) = expectTerminal c st
parse g Failure (NonTerminal nt actuals) = Failure
parse g (Parsing text store) (NonTerminal nt actuals) =
    let
        formals = getFormals nt g
        newStore = createNewStore formals actuals empty
        st' = Parsing text newStore
        expr' = production nt g
    in
        case parse g st' expr' of
            (Parsing text' modifiedStore) ->
                let
                    reconciledStore = reconcileStore formals actuals store modifiedStore
                in
                    Parsing text' reconciledStore
            Failure ->
                Failure
    where
        createNewStore [] [] newStore = newStore
        createNewStore (f:fs) (a:as) newStore =
            -- Populate f in the new store with a's value in the existing store
            case fetch a store of
                Just val ->
                    let
                        newStore' = insert f val newStore
                    in
                        createNewStore fs as newStore'
                Nothing ->
                    createNewStore fs as newStore
        reconcileStore [] [] store modifiedStore = store
        reconcileStore (f:fs) (a:as) store modifiedStore =
            -- Alter a in the store with f's value in the modified store
            case fetch f modifiedStore of
                Just val ->
                    let
                        store' = insert a val store
                    in
                        reconcileStore fs as store' modifiedStore
                Nothing ->
                    reconcileStore fs as store modifiedStore

parse g st@(Parsing text store) (Constraint cstr) =
    case applyConstraint cstr store of
        Just store' ->
            Parsing text store'
        Nothing ->
            Failure

applyConstraint :: Constraint -> Store -> Maybe Store
applyConstraint (UnifyConst v i) st =
    case fetch v st of
        Just value ->
            if value == i then Just st else Nothing
        Nothing ->
            Just $ insert v i st
applyConstraint (UnifyVar v w) st =
    case (fetch v st, fetch w st) of
        (Just vValue, Just wValue) ->
            if vValue == wValue then Just st else Nothing
        (Just vValue, Nothing) ->
            Just $ insert w vValue st
        (Nothing, Just wValue) ->
            Just $ insert v wValue st
        (Nothing, Nothing) ->
            Just st
applyConstraint (Inc v i) st =
    Just $ update (\i -> Just (i + 1)) v st
applyConstraint (Dec v i) st =
    Just $ update (\i -> Just (i - 1)) v st
applyConstraint (GreaterThan v i) st =
    case fetch v st of
        Just value ->
            if value > i then Just st else Nothing
        Nothing ->
            Nothing
applyConstraint (LessThan v i) st =
    case fetch v st of
        Just value ->
            if value < i then Just st else Nothing
        Nothing ->
            Nothing


parseFrom :: Grammar -> String -> ParseState
parseFrom g s = parse g (Parsing s empty) (production (startSymbol g) g)
