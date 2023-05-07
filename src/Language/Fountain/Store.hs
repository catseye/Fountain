module Language.Fountain.Store where

import qualified Data.Map as Map

import Language.Fountain.Constraint
import Language.Fountain.Loader (parseConstConstraint)  -- FIXME an unfortunate coupling


data Store = Store {
    scopes :: [Map.Map Variable Integer],
    events :: [String]
  } deriving (Show, Ord, Eq)


empty = Store{ scopes=[Map.empty], events=[] }

fetch k st = lookup k $ scopes st where
    lookup k [] = Nothing
    lookup k (table:rest) =
        case Map.lookup k table of
            Just v -> Just v
            Nothing -> lookup k rest

insert k v st = st{ scopes=insertTop k v $ scopes st, events=("insert":events st) }
    where insertTop k v (table:rest) = (Map.insert k v table:rest)

-- TODO: this needs to update the layer where the variable is found
update f k st = st{ scopes=updateTop f k $ scopes st, events=("update":events st) }
    where updateTop f k (table:rest) = (Map.update f k table:rest)

pushScope st = st{ scopes=(Map.empty:scopes st) }
popScope st = st{ scopes=tail $ scopes st }

constructStore :: [String] -> Store
constructStore [] = empty
constructStore (constConstrainer:rest) =
    let
        (k, v) = parseConstConstraint constConstrainer
    in
        insert k v $ constructStore rest
