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

data Expr = Val Int | App Op Expr Expr
instance Show Expr where
  show (Val x) = show x
  show (App op x y) = "(" ++ show x ++ show op ++ show y ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r