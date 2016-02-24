{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

import qualified BinTree
import Data.Monoid

class Set a t where
    emptySet :: t a
    find :: a -> t a -> Bool
    insert :: a -> t a -> t a
    delete :: a -> t a -> t a
    next :: a -> t a -> Maybe a

    toList :: t a -> [a]

    fromList :: [a] -> t a
    fromList xs = foldr insert emptySet xs

instance Set a t => Monoid (t a) where
    mempty = emptySet
    mappend a b = fromList $ (toList a) ++ (toList b)
    mconcat xs = fromList $ concat $ map toList xs

instance Ord a => Set a BinTree.BinTree where
    emptySet = BinTree.empty
    find x t = BinTree.find x t
    insert x t = BinTree.insert x t
    delete x t = BinTree.delete x t
    next x t = BinTree.next t x 
    toList t = foldr (:) [] t