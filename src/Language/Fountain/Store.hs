module Language.Fountain.Store where

import qualified Data.Map as Map

import Language.Fountain.Constraint
import Language.Fountain.Loader (parseConstConstraint)  -- FIXME an unfortunate coupling


data Store = Store {
    table :: Map.Map Variable Integer,
    events :: [String]
  } deriving (Show, Ord, Eq)


empty :: Store
empty = Store{ table=Map.empty, events=[] }

fetch :: Variable -> Store -> Maybe Integer
fetch k st = Map.lookup k (table st)

insert :: Variable -> Integer -> Store -> Store
insert k v st = st{ table=Map.insert k v (table st), events=("insert":events st) }

update :: (Integer -> Maybe Integer) -> Variable -> Store -> Store
update f k st = st{ table=Map.update f k (table st), events=("update":events st) }

constructStore :: [String] -> Store
constructStore [] = empty
constructStore (constConstrainer:rest) =
    let
        (k, v) = parseConstConstraint constConstrainer
    in
        insert k v $ constructStore rest

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

ceval :: CExpr -> Store -> Maybe Integer
ceval (CInt i) _ = Just i
ceval (CVar v) st = fetch v st
