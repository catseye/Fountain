module Language.Fountain.Generator (constructState, generateFrom, obtainResult) where

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


gen :: Grammar -> GenState -> Expr -> GenState

gen g st (Seq s) = genSeq g st s where
    genSeq g st [] = st
    genSeq g st (e : rest) =
        case gen g st e of
            Failure -> Failure
            st'     -> genSeq g st' rest

-- FIXME: this should look at all the alts and
-- each of those alts should start with pre-conditions
-- and we should narrow which one down based on that.
-- Then pick randomly.
gen g st (Alt s) = genAlt g st s where
    genAlt g st [] = Failure
    genAlt g st (e : rest) =
        case gen g st e of
            Failure -> genAlt g st rest
            st'     -> st'

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

gen g st (Term t) = genTerm g st t where
    genTerm g st t@(T c) = genTerminal c st
    genTerm g st nt@(NT _) = gen g st (production nt g)

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
applyConstraint (Arb v) st =
    -- TODO not always 5 :)
    Just $ insert v 5 st
applyConstraint (Inc v i) st =
    Just $ update (\i -> Just (i + 1)) v st
applyConstraint (Dec v i) st =
    Just $ update (\i -> Just (i - 1)) v st


constructState :: [String] -> GenState
constructState _ = (Generating "" empty)  -- FIXME

generateFrom :: Grammar -> GenState ->  GenState
generateFrom g state = revgen $ gen g state (production (startSymbol g) g)
    where
        revgen (Generating s a) = Generating (reverse s) a
        revgen other = other
