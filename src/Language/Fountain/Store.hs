module Language.Fountain.Store where

import qualified Data.Map as Map

import Language.Fountain.Constraint


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
