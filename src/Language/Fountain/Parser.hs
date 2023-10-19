module Language.Fountain.Parser (constructState, parseFrom, obtainResult) where

import Language.Fountain.Grammar
import Language.Fountain.Constraint
import Language.Fountain.Store


data ParseState = Parsing String Store
                | Failure
    deriving (Show, Ord, Eq)


expectTerminal :: Char -> ParseState -> ParseState
expectTerminal tc (Parsing (c:cs) a) = if c == tc then (Parsing cs a) else Failure
expectTerminal _tc (Parsing [] _a) = Failure
expectTerminal _tc Failure = Failure

obtainResult :: ParseState -> Either String String
obtainResult (Parsing s _) = Right s
obtainResult Failure = Left "failure"


parse :: Grammar -> ParseState -> Expr -> ParseState

parse _g Failure _expr = Failure

parse g state (Seq s) = parseSeq state s where
    parseSeq st [] = st
    parseSeq st (e : rest) =
        case parse g st e of
            Failure -> Failure
            st'     -> parseSeq st' rest

-- Hello, Mrs Backtracking Alternation!
parse g state (Alt True choices) = parseAlt state choices where
    -- FIXME: select only the choices that could possibly apply
    parseAlt _st [] = Failure
    parseAlt st (e : rest) =
        case parse g st e of
            Failure -> parseAlt st rest
            st'     -> st'

-- Hello, Mrs Non-Backtracking Alternation!
parse g state@(Parsing _str store) expr@(Alt False choices) =
    case missingPreConditions choices of
        missing@(_:_) ->
            error ("No pre-condition present on these Alt choices: " ++ (show missing) ++ " of this: " ++ (show expr))
        [] ->
            let
                preConditionedChoices = map (\x -> (getPreCondition x, x)) choices
                isApplicableChoice (Just c, _) = canApplyConstraint c store
                isApplicableChoice _ = False
                applicableChoices = filter (isApplicableChoice) preConditionedChoices
            in
                parseAlt state applicableChoices where
            where
                parseAlt _st [] = Failure
                -- we ignore the constraint here because it will be found and applied when we descend into e
                parseAlt st [(_, e)] = parse g st e
                parseAlt _st other =
                    error ("Multiple pre-conditions are satisfied in Alt: " ++ (show other))

parse g state (Loop l _) = parseLoop state l where
    parseLoop st e =
        case parse g st e of
            Failure -> st
            st'     -> parseLoop st' e

parse _g state (Terminal c) = expectTerminal c state

parse g (Parsing text store) (NonTerminal nt actuals) =
    let
        formals = getFormals nt g
        newStore = updateStore actuals formals store empty
        st' = Parsing text newStore
        expr' = production nt g
    in
        case parse g st' expr' of
            Parsing text' modifiedStore ->
                let
                    reconciledStore = updateStore formals actuals modifiedStore store
                in
                    Parsing text' reconciledStore
            Failure ->
                Failure

parse _g (Parsing text store) (Constraint cstr) =
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
applyConstraint (Inc v e) st =
    case ceval e st of
        Just delta ->
            Just $ update (\i -> Just (i + delta)) v st
        Nothing ->
            Nothing
applyConstraint (Dec v e) st =
    case ceval e st of
        Just delta ->
            Just $ update (\i -> Just (i - delta)) v st
        Nothing ->
            Nothing
applyConstraint (GreaterThan v e) st = applyRelConstraint (>) v e st
applyConstraint (GreaterThanOrEqual v e) st = applyRelConstraint (>=) v e st
applyConstraint (LessThan v e) st = applyRelConstraint (<) v e st
applyConstraint (LessThanOrEqual v e) st = applyRelConstraint (<=) v e st
applyConstraint (Both c1 c2) st =
    case applyConstraint c1 st of
        Just st' ->
            applyConstraint c2 st'
        Nothing ->
            Nothing

applyRelConstraint op v e st =
    case (fetch v st, ceval e st) of
        (Just value, Just target) ->
            if value `op` target then Just st else Nothing
        _ ->
            Nothing

canApplyConstraint c store =
    case applyConstraint c store of
        Just _  -> True
        Nothing -> False


constructState :: String -> [String] -> ParseState
constructState text initialParams = Parsing text $ constructStore initialParams

parseFrom :: Grammar -> String -> ParseState -> ParseState
parseFrom g start st = parse g st (production start  g)
