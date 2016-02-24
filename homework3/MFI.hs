{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}

module MFI where

import Monad
import MonadFish
import MonadJoin

instance MonadFish m => Monad m where
    return = returnFish

    x >>= f = ((\x -> x) >=> f) x

instance MonadFish m => MonadJoin m where
    returnJoin = returnFish

    join = (\x -> x) >=> (\x -> x)
