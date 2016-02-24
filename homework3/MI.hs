{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}

module MI where

import Monad
import MonadFish
import MonadJoin

instance Monad m => MonadFish m where
    returnFish = return

    f >=> g = \x -> return x >>= f >>= g

instance Monad m => MonadJoin m where
    returnJoin = return

    join x = x >>= \x -> x
