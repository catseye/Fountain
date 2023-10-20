module Language.Fountain.Generator (constructState, generateFrom, obtainResult) where

import Data.Maybe (mapMaybe)
--import Debug.Trace

import Language.Fountain.Store
import Language.Fountain.Constraint
import qualified Language.Fountain.ConstraintStore as CS
import Language.Fountain.Grammar


data GenState = Generating String Store
              | Failure
    deriving (Show, Ord, Eq)

--
-- Utils
--

can (Just _) = True
can Nothing  = False

trace _ x = x

genTerminal c (Generating cs a) = (Generating (c:cs) a)
genTerminal _c Failure = Failure

--
-- Alt choices need preconditions during generation because
-- we need some guidance of which one to pick.
--
getPreCondition :: Expr -> Maybe Constraint
getPreCondition (Seq (x:_)) = getPreCondition x
getPreCondition (Constraint c) = Just c
getPreCondition _ = Nothing

missingPreConditions choices =
    mapMaybe (\x -> case getPreCondition x of
        Just _ -> Nothing
        Nothing -> Just x
      ) choices

--
-- Generator
--

gen :: Grammar -> GenState -> Expr -> GenState

gen _g Failure _expr = Failure

gen g state (Seq s) = genSeq state s where
    genSeq st [] = st
    genSeq st (e : rest) =
        case gen g st e of
            Failure -> Failure
            st'     -> genSeq st' rest

-- Hello, Mrs Backtracking Alternation!
gen g state (Alt True choices) = genAlt state choices where
    -- Note, we try all the possibilities here, regardless of their preconditions.
    genAlt _st [] = Failure
    genAlt st (e : rest) =
        case gen g st e of
            Failure -> genAlt st rest
            st'     -> st'

-- Hello, Mrs Non-Backtracking Alternation!
gen g state@(Generating _str store) (Alt False choices) =
    -- We look at all the choices; each should start with a pre-condition
    -- determining whether we can select it; and we should narrow down our
    -- choices based on that.
    case missingPreConditions choices of
        missing@(_:_) ->
            error ("No pre-condition present on these Alt choices: " ++ (depictExprs missing))
        [] ->
            let
                preConditionedChoices = map (\x -> (getPreCondition x, x)) choices
                isApplicableChoice (Just c, _) = can $ CS.applyConstraint c store
                isApplicableChoice _ = False
                applicableChoices = filter (isApplicableChoice) preConditionedChoices
            in
                genAlt state applicableChoices
            where
                genAlt _st [] = Failure
                -- we ignore the constraint here because it will be found and applied when we descend into e
                genAlt st [(_, e)] = gen g st e
                genAlt _st other =
                    error ("Multiple pre-conditions are satisfied in Alt: " ++ (depictExprs (map (snd) other)))


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
        trace ("Wend? " ++ (show c) ++ " in " ++ (show st)) $ case CS.applyConstraint c st of
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
    case CS.applyConstraint cstr store of
        Just store' ->
            trace ("OK " ++ (show cstr) ++ " => " ++ (show store')) Generating text store'
            --Generating text store'
        Nothing ->
            trace ("No " ++ (show cstr) ++ " !: " ++ (show store)) Failure
            --Failure

--
-- Usage interface
--

constructState :: [String] -> GenState
constructState initialParams = Generating "" $ CS.constructStore initialParams

generateFrom :: Grammar -> String -> GenState ->  GenState
generateFrom g start state = revgen $ gen g state (production start g)
    where
        revgen (Generating s a) = Generating (reverse s) a
        revgen other = other

obtainResult :: GenState -> Either String String
obtainResult (Generating s _) = Right s
obtainResult Failure = Left "failure"
