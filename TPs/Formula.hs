module Formula
(
  Formula(..)
, makeFormula
, isEmpty
, isSatisfied
, unitClauses
, mostFrequentLiteral
, hasUnsatisfiedClause
, makeExampleFormula
)
where

import qualified Data.Ord as Ord
import qualified Data.List as List
import qualified Literal as L
import qualified Variable as V
import qualified Clause as C

-- | 'Formula' type
type Formula = [C.Clause]

makeFormula :: [C.Clause] -> Formula
makeFormula = List.nub . List.map C.makeClause

isEmpty :: Formula -> Bool
isEmpty = List.null

isSatisfied :: Formula -> Bool
isSatisfied = isEmpty

unitClauses :: Formula -> Formula
unitClauses x = filter(\x -> C.size x == 1)
