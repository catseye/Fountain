module Language.Fountain.Store where

import qualified Data.Map as Map

import Language.Fountain.Constraint
import Language.Fountain.Loader (parseConstConstraint)  -- FIXME an unfortunate coupling


data Store = Store {
    table :: Map.Map Variable Integer,
    events :: [String]
  } deriving (Show, Ord, Eq)


empty = Store{ table=Map.empty, events=[] }
fetch k st = Map.lookup k (table st)
insert k v st = st{ table=Map.insert k v (table st), events=("insert":events st) }
update f k st = st{ table=Map.update f k (table st), events=("update":events st) }

constructStore :: [String] -> Store
constructStore [] = empty
constructStore (constConstrainer:rest) =
    let
        (k, v) = parseConstConstraint constConstrainer
    in
        insert k v $ constructStore rest
