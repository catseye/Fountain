module Language.Fountain.Store where

import qualified Data.Map as Map

import Language.Fountain.Constraint


data Store = Store {
    table :: Map.Map Variable Integer,
    events :: [String]
  } deriving (Show, Ord, Eq)


empty = Store{ table=Map.empty, events=[] }
fetch k st = Map.lookup k (table st)
insert k v st = st{ table=Map.insert k v (table st), events=("insert":events st) }
update f k st = st{ table=Map.update f k (table st), events=("update":events st) }
