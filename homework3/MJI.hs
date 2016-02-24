{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}

module MJI where

import Control.Applicative

import Monad
import MonadFish
import MonadJoin

instance MonadJoin m => Monad m where
    return = returnJoin

    x >>= f = join (return f <*> x)

instance MonadJoin m => MonadFish m where
    returnFish = returnJoin

    f >=> g = \x -> join (return g <*> (join (return f <*> return x)))
