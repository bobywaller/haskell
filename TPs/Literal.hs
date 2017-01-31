module Literal
(
  Literal
, isPositiveLiteral
, isNegativeLiteral
, toVariable
)
where

import qualified Variable as V

-- | 'Literal' type
type Literal = Int


isPositiveLiteral :: Literal -> Bool
isPositiveLiteral = (> 0)

isNegativeLiteral :: Literal -> Bool
isNegativeLiteral = not . isPositiveLiteral

toVariable :: Literal -> V.Variable
toVariable = abs
