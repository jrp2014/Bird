{-# LANGUAGE UnicodeSyntax #-}

module Nexus where

data Tree a
  = Leaf a
  | Node [Tree a]

{-
fold :: (Either a [b] -> b) -> Tree a -> b
fold f t =
  case t of
    Leaf x -> f (Left x)
    Node ts -> f (Right (map (fold f) ts))
-}
fold :: (a -> b) -> ([b] -> b) -> Tree a -> b
fold f g (Leaf x) = f x
fold f g (Node ts) = g (map (fold f g) ts)

{-
unfold :: (b -> Either a [b]) -> b -> Tree a
unfold g x =
  case g x of
    Left y -> Leaf y
    Right xs -> Node (map (unfold g) xs)
-}
unfold :: (b -> Bool) -> (b -> a) -> (b -> [b]) -> b -> Tree a
unfold p v h x =
  if p x
    then Leaf (v x)
    else Node (map (unfold p v h) (h x))

{-
hylo :: (Either a1 [b] -> b) -> (a2 -> Either a1 [a2]) -> a2 -> b
hylo f g x =
  case g x of
    Left y -> f (Left y)
    Right xs -> f (Right (map (hylo f g) xs))
-}
--hylo p v f g h x = fold f g . unfold p v h
