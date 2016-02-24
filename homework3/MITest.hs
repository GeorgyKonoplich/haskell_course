{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}

module MITest where

import Data.List
import Data.Function

import MI
import Monad
import MonadFish
import MonadJoin

instance Monad [] where
    return x = [x]

    x >>= f = concat $ map f x
