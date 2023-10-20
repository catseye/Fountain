module Language.Fountain.Parser (constructState, parseFrom, obtainResult) where

import Data.Maybe (mapMaybe)

import Language.Fountain.Grammar
import Language.Fountain.Constraint
import Language.Fountain.Store


data ParseState = Parsing String Store
                | Failure
    deriving (Show, Ord, Eq)

--
-- Utils
--

can (Just _) = True
can Nothing  = False

expectTerminal :: Char -> ParseState -> ParseState
expectTerminal tc (Parsing (c:cs) a) = if c == tc then (Parsing cs a) else Failure
expectTerminal _tc (Parsing [] _a) = Failure
expectTerminal _tc Failure = Failure

--
-- Alt choices need preconditions during parsing because it helps
-- efficiency by preventing unnecessary backtracking.  But note that
-- we need a more refined notion in parsing than in generation,
-- because a terminal counts as a precondition.
--
getPreCondition :: Expr -> Maybe Constraint
getPreCondition (Seq (x:_)) = getPreCondition x
getPreCondition (Constraint c) = Just c
getPreCondition (Terminal c) = Just $ Lookahead s where s = [c]
getPreCondition _ = Nothing

missingPreConditions choices =
    mapMaybe (\x -> case getPreCondition x of
        Just _ -> Nothing
        Nothing -> Just x
      ) choices

--
-- Parser
--

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
parse g state (Alt False choices) =
    case missingPreConditions choices of
        missing@(_:_) ->
            error ("No pre-condition present on these Alt choices: " ++ (depictExprs missing))
        [] ->
            let
                preConditionedChoices = map (\x -> (getPreCondition x, x)) choices
                isApplicableChoice (Just c, _) = can $ applyConstraint c state
                isApplicableChoice _ = False
                applicableChoices = filter (isApplicableChoice) preConditionedChoices
            in
                parseAlt state applicableChoices where
            where
                parseAlt _st [] = Failure
                -- we ignore the constraint here because it will be found and applied when we descend into e
                parseAlt st [(_, e)] = parse g st e
                parseAlt (Parsing _str store) other =
                    error ("Multiple pre-conditions are satisfied in Alt: " ++ (depictExprs (map (snd) other)) ++ ", with state: " ++ show store)
                parseAlt _ _ = Failure

parse g state (Loop l []) = parseLoop state l where
    parseLoop st e =
        case parse g st e of
            Failure -> st
            st'     -> parseLoop st' e

parse _g _state (Loop l (_:_)) = error ("Parsing can't handle decorated Loops: " ++ depictExpr l)

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

parse _g state@(Parsing s _) (Constraint cstr) =
    case applyConstraint cstr state of
        Just store' ->
            Parsing s store'
        Nothing ->
            Failure


-- FIXME: this is goofy.  It takes the entire state but returns only the store (maybe)
applyConstraint :: Constraint -> ParseState -> Maybe Store
applyConstraint _ Failure = Nothing
applyConstraint (UnifyConst v i) (Parsing _ st) =
    case fetch v st of
        Just value ->
            if value == i then Just st else Nothing
        Nothing ->
            Just $ insert v i st
applyConstraint (UnifyVar v w) (Parsing _ st) =
    case (fetch v st, fetch w st) of
        (Just vValue, Just wValue) ->
            if vValue == wValue then Just st else Nothing
        (Just vValue, Nothing) ->
            Just $ insert w vValue st
        (Nothing, Just wValue) ->
            Just $ insert v wValue st
        (Nothing, Nothing) ->
            Just st
applyConstraint (Inc v e) (Parsing _ st) =
    case ceval e st of
        Just delta ->
            Just $ update (\i -> Just (i + delta)) v st
        Nothing ->
            Nothing
applyConstraint (Dec v e) (Parsing _ st) =
    case ceval e st of
        Just delta ->
            Just $ update (\i -> Just (i - delta)) v st
        Nothing ->
            Nothing
applyConstraint (Both c1 c2) state@(Parsing s _st) =
    case applyConstraint c1 state of
        Just st' ->
            applyConstraint c2 (Parsing s st')
        Nothing ->
            Nothing
applyConstraint (Lookahead s) (Parsing (c:_) st) =
    if s == [c] then Just st else Nothing
applyConstraint (Lookahead _) _ =
    Nothing
applyConstraint (GreaterThan v e) state = applyRelConstraint (>) v e state
applyConstraint (GreaterThanOrEqual v e) state = applyRelConstraint (>=) v e state
applyConstraint (LessThan v e) state = applyRelConstraint (<) v e state
applyConstraint (LessThanOrEqual v e) state = applyRelConstraint (<=) v e state

applyRelConstraint _ _ _ Failure = Nothing
applyRelConstraint op v e (Parsing _ st) =
    case (fetch v st, ceval e st) of
        (Just value, Just target) ->
            if value `op` target then Just st else Nothing
        _ ->
            Nothing

--
-- Usage interface
--

constructState :: String -> [String] -> ParseState
constructState text initialParams = Parsing text $ constructStore initialParams

parseFrom :: Grammar -> String -> ParseState -> ParseState
parseFrom g start st = parse g st (production start  g)

obtainResult :: ParseState -> Either String String
obtainResult (Parsing s _) = Right s
obtainResult Failure = Left "failure"
