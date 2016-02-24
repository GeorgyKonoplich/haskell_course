{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}

module Monad where

import Control.Applicative

class Applicative m => Monad m where
    return :: a -> m a

    (>>=) :: m a -> (a -> m b) -> m b

infixl 1 >>=
