module Language.Fountain.Parser (constructState, parseFrom, obtainResult) where

import Data.Maybe (mapMaybe)

import Language.Fountain.Store
import Language.Fountain.Constraint
import qualified Language.Fountain.ConstraintStore as CS
import Language.Fountain.Grammar


data ParseState = Parsing String Store
                | Failure
    deriving (Show, Ord, Eq)

--
-- Utils
--

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

getApplicableChoices state choices =
    let
        preConditionedChoices = map (\x -> (getPreCondition x, x)) choices
        isApplicableChoice (Just c, _) = case applyConstraint c state of
            Failure -> False
            _ -> True
        isApplicableChoice _ = False
    in
        filter (isApplicableChoice) preConditionedChoices

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
    -- Note, we try all the possibilities here, regardless of their preconditions.
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
            parseAlt state (getApplicableChoices state choices)
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

parse _g state (Constraint cstr) = applyConstraint cstr state


applyConstraint :: Constraint -> ParseState -> ParseState
applyConstraint _ Failure = Failure
applyConstraint (Lookahead s) state@(Parsing (c:_) _) =
    if s == [c] then state else Failure
applyConstraint (Lookahead _) _ = Failure
applyConstraint other (Parsing s store) =
    case CS.applyConstraint other store of
        Just store' ->
            Parsing s store'
        Nothing ->
            Failure

--
-- Usage interface
--

constructState :: String -> [String] -> ParseState
constructState text initialParams = Parsing text $ CS.constructStore initialParams

parseFrom :: Grammar -> String -> ParseState -> ParseState
parseFrom g start st = parse g st (production start  g)

obtainResult :: ParseState -> Either String String
obtainResult (Parsing s _) = Right s
obtainResult Failure = Left "failure"
