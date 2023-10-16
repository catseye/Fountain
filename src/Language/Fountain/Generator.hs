module Language.Fountain.Generator (constructState, generateFrom, obtainResult) where

import Data.Maybe (mapMaybe)

import Language.Fountain.Grammar
import Language.Fountain.Constraint
import Language.Fountain.Store


data GenState = Generating String Store
              | Failure
    deriving (Show, Ord, Eq)


genTerminal c (Generating cs a) = (Generating (c:cs) a)

obtainResult :: GenState -> Either String String
obtainResult (Generating s _) = Right s
obtainResult Failure = Left "failure"

--
-- Alt choices need preconditions because in generating, unlike parsing,
-- we need some guidance of which one to pick
--
getPreCondition :: Expr -> Maybe Constraint
getPreCondition (Seq (x:xs)) = getPreCondition x
getPreCondition (Constraint c) = Just c
getPreCondition x = Nothing

missingPreConditions choices =
    mapMaybe (\x -> case getPreCondition x of
        Just _ -> Nothing
        Nothing -> Just x
      ) choices


gen :: Grammar -> GenState -> Expr -> GenState

gen g st (Seq s) = genSeq g st s where
    genSeq g st [] = st
    genSeq g st (e : rest) =
        case gen g st e of
            Failure -> Failure
            st'     -> genSeq g st' rest

-- We look at all the choices; each should start with a pre-condition
-- determining whether we can select it; and we should narrow down our
-- choices based on that. (Then pick randomly?  Or insist deterministic?)
gen g st@(Generating str store) (Alt choices) =
    case missingPreConditions choices of
        missing@(_:_) ->
            error ("No pre-condition present on these Alt choices: " ++ (show missing))
        [] ->
            let
                preConditionedChoices = map (\x -> (getPreCondition x, x)) choices
                applicableChoices = filter (\(Just c, x) -> canApplyConstraint c store) preConditionedChoices
            in
                genAlt g st applicableChoices where
                genAlt g st [] = Failure
                -- we ignore the constraint because it will apply the constraint once descending into e
                genAlt g st [(_, e)] = gen g st e
                genAlt g st other =
                    error ("Multiple pre-conditions are satisfied in Alt: " ++ (show other))

gen g state (Loop l postconditions) =
    genLoop g state l (assertThereAreSome postconditions) where
        genLoop g state e postconditions =
            case gen g state e of
                Failure -> state
                state'@(Generating str store)  ->
                    case checkLimit postconditions store of
                        -- All postconditions met, terminate the loop.
                        Just store'  -> Generating str store'
                        -- Not all postconditions met -- go 'round again
                        Nothing      -> genLoop g state' e postconditions
        assertThereAreSome [] = error "No postconditions defined for this Loop"
        assertThereAreSome pcs = pcs
        checkLimit [] st = Just st
        checkLimit (c:cs) st =
            case applyConstraint c st of
                Nothing -> Nothing
                Just st' -> checkLimit cs st'

gen g st (Terminal c) = genTerminal c st
gen g Failure (NonTerminal nt actuals) = Failure
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

gen g st@(Generating text store) (Constraint cstr) =
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
applyConstraint (GreaterThan v e) st =
    case (fetch v st, ceval e st) of
        (Just value, Just target) ->
            if value > target then Just st else Nothing
        _ ->
            Nothing
applyConstraint (GreaterThanOrEqual v e) st =
    case (fetch v st, ceval e st) of
        (Just value, Just target) ->
            if value >= target then Just st else Nothing
        _ ->
            Nothing
applyConstraint (LessThan v e) st =
    case (fetch v st, ceval e st) of
        (Just value, Just target) ->
            if value < target then Just st else Nothing
        _ ->
            Nothing
applyConstraint (LessThanOrEqual v e) st =
    case (fetch v st, ceval e st) of
        (Just value, Just target) ->
            if value <= target then Just st else Nothing
        _ ->
            Nothing

canApplyConstraint c store =
    case applyConstraint c store of
        Just _  -> True
        Nothing -> False

constructState :: [String] -> GenState
constructState initialParams = Generating "" (constructStore initialParams)

generateFrom :: Grammar -> GenState ->  GenState
generateFrom g state = revgen $ gen g state (production (startSymbol g) g)
    where
        revgen (Generating s a) = Generating (reverse s) a
        revgen other = other
