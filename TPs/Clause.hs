module Clause
(
  Clause
, makeClause
, isEmpty
, size
, isUnit
)
where

import qualified Data.List as List
import qualified Literal as L

-- | 'Clause' type
type Clause = [L.Literal]

makeClause :: [L.Literal] -> Clause
makeClause = List.delete(0) . List.sort . List.nub

isEmpty :: Clause -> Bool
isEmpty = List.null

size :: Clause -> Int
size = List.length

isUnit :: Clause -> Bool
isUnit x = size x == 1
