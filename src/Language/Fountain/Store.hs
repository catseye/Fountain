module Language.Fountain.Store (
    Store, empty, fetch, insert, update, updateStore,
    constructStore, ceval, applyConstraint
) where

--
-- Variable store.  This is not very generic; it contains values of variables
-- (and some other stuff) and knows how to apply constraints to the store.
--

import qualified Data.Map as Map

import Language.Fountain.Constraint
import Language.Fountain.Loader (parseConstConstraint)


type Store = Map.Map Variable Integer

empty = Map.empty
fetch = Map.lookup
insert = Map.insert
update f k m = Map.update f k m

updateStore :: [Variable] -> [Variable] -> Store -> Store -> Store
updateStore [] [] _sourceStore destStore = destStore
updateStore [] _ _ _ = error "Variable lists must be same length"
updateStore _ [] _ _ = error "Variable lists must be same length"
updateStore (sourceKey:sourceKeys) (destKey:destKeys) sourceStore destStore =
    -- Populate destKey in the new store with the value at sourceKey in the sourceStore
    let
        destStore' = case fetch sourceKey sourceStore of
            Just val -> insert destKey val destStore
            Nothing -> destStore
    in
        updateStore sourceKeys destKeys sourceStore destStore'


constructStore :: [String] -> Store
constructStore [] = empty
constructStore (constConstrainer:rest) =
    let
        (k, v) = parseConstConstraint constConstrainer
    in
        insert k v $ constructStore rest

ceval :: CExpr -> Store -> Maybe Integer
ceval (CInt i) _ = Just i
ceval (CVar v) st = fetch v st

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
