module Countdown where

data Expr
  = Num Int
  | App Op Expr Expr

data Op
  = Add
  | Sub
  | Mul
  | Div

type Value = Int

display :: Show a => (Expr,  a) -> IO()
display (e, v) = print $ display' e ++ " = " ++ show v

display' :: Expr -> String
display' (Num x) = show x
display' (App op e1 e2) =
  '(' : display' e1 ++ (display'' op : display' e2 ++ ")")

display'' :: Op -> Char
display'' Add = '+'
display'' Sub = '-'
display'' Mul = '*'
display'' Div = '/'

countdown1 :: Int -> [Int] -> (Expr, Value)
countdown1 n = nearest n . concatMap mkExprs . subseqs

value :: Expr -> Value
value (Num x       ) = x
value (App op e1 e2) = apply op (value e1) (value e2)

apply :: Op -> Value -> Value -> Value
apply Add = (+)
apply Sub = (-)
apply Mul = (*)
apply Div = div


legal :: Op -> Value -> Value -> Bool
legal Add v1 v2 = True
legal Sub v1 v2 = v2 < v1
legal Mul v1 v2 = True
legal Div v1 v2 = v1 `mod` v2 == 0

subseqs :: [a] -> [[a]]
subseqs [x     ] = [[x]]
subseqs (x : xs) = xss ++ [x] : map (x :) xss where xss = subseqs xs

mkExprs :: [Int] -> [(Expr, Value)]
mkExprs [x] = [(Num x, x)]
mkExprs xs =
  [ ev
  | (ys, zs) <- unmerges xs
  , ev1      <- mkExprs ys
  , ev2      <- mkExprs zs
  , ev       <- combine ev1 ev2
  ]

unmerges :: [a] -> [([a], [a])]
unmerges [x, y]   = [([x], [y]), ([y], [x])]
unmerges (x : xs) = [([x], xs), (xs, [x])] ++ concatMap (add x) (unmerges xs)
  where add x (ys, zs) = [(x : ys, zs), (ys, x : zs)]

combine :: (Expr, Value) -> (Expr, Value) -> [(Expr, Value)]
combine (e1, v1) (e2, v2) =
  [ (App op e1 e2, apply op v1 v2) | op <- ops, legal op v1 v2 ]
  where ops = [Add, Sub, Mul, Div]

nearest :: (Ord a1, Num a1) => a1 -> [(a2, a1)] -> (a2, a1)
nearest n ((e, v) : evs) = if d == 0 then (e, v) else search n d (e, v) evs
  where d = abs (n - v)

search :: (Ord a1, Num a1) => a1 -> a1 -> (a2, a1) -> [(a2, a1)] -> (a2, a1)
search n d ev [] = ev
search n d ev ((e, v) : evs) | d' == 0 = (e, v)
                             | d' < d  = search n d' (e, v) evs
                             | d' >= d = search n d ev evs
  where d' = abs (n - v)
