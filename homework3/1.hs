import Data.Maybe

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show)

next :: (Ord a) => (a, Tree a, Tree a) -> Maybe (a, Tree a, Tree a)
next (k, Leaf, _) = Nothing
next (k, (Node x l r), t)
    | k < x && isNothing nextis  = Just (x, t, t)
    | k < x                      = nextis
    | k >= x                     = next (k, r, t)
    where nextis = next (k, l, t)
next a =  Just a

hNextN :: (Ord a) => Int -> Maybe (a, Tree a, Tree a) -> Maybe (a,Tree a, Tree a)
hNextN 0 m = m
hNextN n m
    | n < 0     = Nothing
    | otherwise = hNextN (n-1) (m >>= next)

nextN :: (Ord a) => Int -> a -> Tree a -> Maybe a
nextN n k t = case hNextN n (Just (k, t, t)) of
                   Nothing -> Nothing
                   Just (a, t1, t2) -> Just a

abc = (Node 6 (Node 3 (Node 2 Leaf Leaf ) (Node 4 Leaf Leaf)) Leaf)
