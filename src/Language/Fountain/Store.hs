module Language.Fountain.Store where

import qualified Data.Map as Map

import Language.Fountain.Constraint
import Language.Fountain.Loader (parseConstConstraint)  -- FIXME an unfortunate coupling


data Store = Store {
    scopes :: [Map.Map Variable Integer],
    events :: [String]
  } deriving (Show, Ord, Eq)


empty = Store{ scopes=[Map.empty], events=[] }
fetch k st = Map.lookup k (head $ scopes st)
insert k v st = st{ scopes=insertTop k v $ scopes st, events=("insert":events st) }
update f k st = st{ scopes=updateTop f k $ scopes st, events=("update":events st) }

insertTop k v (table:rest) = (Map.insert k v table:rest)
updateTop f k (table:rest) = (Map.update f k table:rest)

constructStore :: [String] -> Store
constructStore [] = empty
constructStore (constConstrainer:rest) =
    let
        (k, v) = parseConstConstraint constConstrainer
    in
        insert k v $ constructStore rest
