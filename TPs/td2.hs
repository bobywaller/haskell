import Data.List

-- Question 1
mirror :: Eq a => [a] -> [a] -> Bool
mirror xs ys = xs == reverse ys

permute' :: Ord a => [a] -> [a] -> Bool
permute' xs ys = sort xs == sort ys

-- Question 2
reversal :: Int -> Int -> [a] -> [a]
reversal i j xs = ps ++ reverse ys ++ ss
where
  (ps, ss') = splitAt i xs
  (ys, ss) = splitAt (j-i+1) ss'

reversal' :: Int -> Int -> [a] -> [a]
reversal' i l = reversal i (i+l-1)

prefixReversal :: Int -> [a] -> [a]
prefixReversal i = reversal' 0 i-1

suffixReversal :: Int -> [a] -> [a]
suffixReversal i xs = reversal' i ((length xs) - i) xs 
