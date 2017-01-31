import qualified Data.Maybe as Maybe

-- Data defs
data BinIntTree = Empty | Branch BinIntTree Int BinIntTree
  deriving (Show)

indent :: [String] -> [String]
indent = map (" " ++)

layoutTree :: BinIntTree -> [String]
layoutTree Empty = []
layoutTree (Branch left x right) =
  indent (layoutTree right) ++
  [show x] ++
  indent (layoutTree left)

prettyTree :: BinIntTree -> String
prettyTree = unlines . layoutTree

-----------------------------------------------

binIntTreeExample :: BinIntTree
binIntTreeExample = b10
  where
    b5 = Branch Empty 5 Empty
    b8 = Branch b5 8 Empty
    b3 = Branch Empty 3 Empty
    b4 = Branch b3 4 b8
    b15 = Branch Empty 15 Empty
    b20 = Branch b15 20 Empty
    b10 = Branch b4 10 b20

--------------------------------------------------

emptyBinIntTree :: BinIntTree
emptyBinIntTree = Empty

--------------------------------------------------

sizeBinIntTree :: Num a => BinIntTree -> a
sizeBinIntTree Empty = 0
sizeBinIntTree (Branch l _ r) = 1 + sizeLeft + sizeRight
  where
    sizeLeft = sizeBinIntTree l
    sizeRight = sizeBinIntTree r

--------------------------------------------------

maxBinIntTree :: BinIntTree -> Int
maxBinIntTree Empty = minBound
maxBinIntTree (Branch l x r) = maximum [maxLeft, x, maxRight]
  where
    maxLeft = maxBinIntTree l
    maxRight = maxBinIntTree r

---------------------------------------------------

minBinIntTree :: BinIntTree -> Int
minBinIntTree Empty = maxBound
minBinIntTree (Branch l x r) = minimum [minLeft, x, minRight]
  where
    minLeft = minBinIntTree l
    minRight = minBinIntTree r

--------------------------------------------------

genBinIntTree :: BinIntTree -> ([Int] -> Int) -> Int -> Int
genBinIntTree Empty _ b = b
genBinIntTree (Branch l x r) f b = f [genLeft, b, genRight]
  where
    genLeft = genBinIntTree l f b
    genRight = genBinIntTree r f b

minBinIntTree' :: BinIntTree -> Int
minBinIntTree' t = genBinIntTree t minimum maxBound

maxBinIntTree' :: BinIntTree -> Int
maxBinIntTree' t = genBinIntTree t maximum minBound

------------------------------------------------

maxBinIntTree'' :: BinIntTree -> Maybe Int
maxBinIntTree'' Empty = Nothing
maxBinIntTree'' (Branch l x r) = maximum ms
  where
    maxLeft = maxBinIntTree'' l
    maxRight = maxBinIntTree'' r
    ms = [maxLeft, Just x, maxRight]

minBinintTree'' :: BinIntTree -> Maybe Int
minBinintTree'' Empty = Nothing
minBinintTree'' (Branch l x r) = minimum ms
  where
    minLeft = minBinintTree'' l
    minRight = minBinintTree'' r
    ms = [minLeft, Just x, minRight]

---------------------------------------------------h

heightBinIntTree :: (Ord a, Num a) => BinIntTree -> a
heightBinIntTree Empty = 0
heightBinIntTree (Branch l _ r) = 1 + max heightLeft heightRight
  where
    heightLeft = heightBinIntTree l
    heightRight = heightBinIntTree r

-----------------------------------------------------
