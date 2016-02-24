{-# LANGUAGE NoImplicitPrelude #-}

module HW4 where

import Data.Foldable
import Data.Functor
import Data.Traversable
import Control.Applicative (Applicative, pure, (<*>))

import Data.Function
import Data.Monoid

import Tree

newtype Identity a = Identity a

data Either a b = Left a | Right b

data Const a m = Const a

data Pair a b = Pair a b

-- Identity

instance Foldable Identity where
    foldMap f (Identity a) = f a

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Traversable Identity where
    sequenceA (Identity a) = fmap Identity a

instance Applicative Identity where
    pure = Identity

    (Identity f) <*> (Identity x) = Identity (f x)

-- Tree

instance Foldable Tree where
    foldMap _ Leaf = mempty
    foldMap f (Node a l r) = mappend (foldMap f l) $ mappend (f a) $ foldMap f r

instance Functor Tree where
    fmap f Leaf = Leaf
    fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

instance Traversable Tree where
    sequenceA Leaf = pure Leaf
    sequenceA (Node a l r) = Node <$> a <*> (sequenceA l) <*> (sequenceA r)

instance Applicative Tree where
    pure x = Node x (pure x) (pure x)

    (Node f l r) <*> (Node x l1 r1) = Node (f x) (l <*> l1) (r <*> r1)

-- Either a

instance Foldable (Either a) where
    foldr f z (Right x) = f x z
    foldr _ z _ = z

instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap _ (Left x) = Left x

instance Traversable (Either a) where
    sequenceA (Right x) = fmap Right x
    sequenceA (Left x) = pure (Left x)

instance Applicative (Either a) where
    pure = Right

    (Right f) <*> x = fmap f x
    (Left x) <*> _ = (Left x)

-- Const m

instance Foldable (Const m) where
    foldr _ x _ = x

instance Functor (Const m) where
    fmap _ (Const x) = Const x

instance Traversable (Const m) where
    sequenceA (Const x) = pure (Const x)

instance Monoid m => Applicative (Const m) where
    pure _ = Const mempty

    (Const x) <*> (Const y) = Const (mappend x y)

-- (,) a

instance Foldable (Pair a) where
    foldr f b (Pair x y) = f y b

instance Functor (Pair a) where
    fmap f (Pair x y) = Pair x (f y)

instance Traversable (Pair a) where
    sequenceA (Pair x y) = fmap (Pair x) y

instance Monoid a => Applicative (Pair a) where
    pure x = Pair mempty x

    (Pair x f) <*> (Pair y z) = Pair (mappend x y) (f z)
