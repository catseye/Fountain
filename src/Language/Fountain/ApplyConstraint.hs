module Language.Fountain.ApplyConstraint where

import Language.Fountain.Store
import Language.Fountain.Constraint


can (Just _) = True
can Nothing  = False

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
applyConstraint (Both c1 c2) st =
    case applyConstraint c1 st of
        Just st' ->
            applyConstraint c2 st'
        Nothing ->
            Nothing
applyConstraint (GreaterThan v e) st = applyRelConstraint (>) v e st
applyConstraint (GreaterThanOrEqual v e) st = applyRelConstraint (>=) v e st
applyConstraint (LessThan v e) st = applyRelConstraint (<) v e st
applyConstraint (LessThanOrEqual v e) st = applyRelConstraint (<=) v e st
applyConstraint other _state = error ("Can't handle this: " ++ show other)

applyRelConstraint op v e st =
    case (fetch v st, ceval e st) of
        (Just value, Just target) ->
            if value `op` target then Just st else Nothing
        _ ->
            Nothing
