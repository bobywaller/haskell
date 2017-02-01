import Data.List

-- arithmitic operations
data Op = Add | Sub | Mul | Div
instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True -- always true
valid Sub x y = x > y -- do not allow for negative integers
valid Mul _ _ = True -- always valid
valid Div x y = mod x y == 0 -- integer division only

calculate :: Op -> Int -> Int -> Int
calculate Add x y = x + y
calculate Sub x y = x - y
calculate Mul x y = x * y
calculate Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr
instance Show Expr where
  show (Val x) = show x
  show (App op x y) = "(" ++ show x ++ show op ++ show y ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [calculate o x y | x <- eval l, y <- eval r, valid o x y]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where yss = subs xs


interleave :: a -> [a] -> [[a]]
interleave x [] = [[]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

ssens :: [a] -> [[a]]
ssens = concat . map perms . subs

solution :: Expr -> [Int] -> Int -> Maybe Expr
solution ns n = [ e | ns' <- subbags ns, e <- exprs ns'), eval e == [n]] 
