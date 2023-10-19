module Language.Fountain.Generator (constructState, generateFrom, obtainResult) where

import Language.Fountain.Grammar
import Language.Fountain.Constraint
import Language.Fountain.Store


data GenState = Generating String Store
              | Failure
    deriving (Show, Ord, Eq)


genTerminal c (Generating cs a) = (Generating (c:cs) a)
genTerminal _c Failure = Failure

obtainResult :: GenState -> Either String String
obtainResult (Generating s _) = Right s
obtainResult Failure = Left "failure"


gen :: Grammar -> GenState -> Expr -> GenState

gen _g Failure _expr = Failure

gen g state (Seq s) = genSeq state s where
    genSeq st [] = st
    genSeq st (e : rest) =
        case gen g st e of
            Failure -> Failure
            st'     -> genSeq st' rest

-- We look at all the choices; each should start with a pre-condition
-- determining whether we can select it; and we should narrow down our
-- choices based on that.
gen g state@(Generating _str store) (Alt False choices) =
    case missingPreConditions choices of
        missing@(_:_) ->
            error ("No pre-condition present on these Alt choices: " ++ (show missing))
        [] ->
            let
                preConditionedChoices = map (\x -> (getPreCondition x, x)) choices
                isApplicableChoice (Just c, _) = canApplyConstraint c store
                isApplicableChoice _ = False
                applicableChoices = filter (isApplicableChoice) preConditionedChoices
            in
                genAlt state applicableChoices
            where
                genAlt _st [] = Failure
                -- we ignore the constraint here because it will be found and applied when we descend into e
                genAlt st [(_, e)] = gen g st e
                genAlt _st other =
                    error ("Multiple pre-conditions are satisfied in Alt: " ++ (show other))

gen _g _state (Alt True _choices) = error "Backtracking alternations during generation not yet implemented"

gen _g _state (Loop _ []) = error "No postconditions defined for this Loop"
gen g state (Loop l postconditions) = genLoop state l where
    genLoop st e =
        case gen g st e of
            Failure -> st
            st'@(Generating str store)  ->
                case checkLimit postconditions store of
                    -- All postconditions met, terminate the loop.
                    Just store'  -> Generating str store'
                    -- Not all postconditions met -- go 'round again
                    Nothing      -> genLoop st' e
    checkLimit [] st = Just st
    checkLimit (c:cs) st =
        case applyConstraint c st of
            Nothing -> Nothing
            Just st' -> checkLimit cs st'

gen _g st (Terminal c) = genTerminal c st
gen g (Generating text store) (NonTerminal nt actuals) =
    let
        formals = getFormals nt g
        newStore = updateStore actuals formals store empty
        st' = Generating text newStore
        expr' = production nt g
    in
        case gen g st' expr' of
            Generating text' modifiedStore ->
                let
                    reconciledStore = updateStore formals actuals modifiedStore store
                in
                    Generating text' reconciledStore
            Failure ->
                Failure

gen _g (Generating text store) (Constraint cstr) =
    case applyConstraint cstr store of
        Just store' ->
            Generating text store'
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


constructState :: [String] -> GenState
constructState initialParams = Generating "" (constructStore initialParams)

generateFrom :: Grammar -> String -> GenState ->  GenState
generateFrom g start state = revgen $ gen g state (production start g)
    where
        revgen (Generating s a) = Generating (reverse s) a
        revgen other = other
