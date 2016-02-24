{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}

module MJITest where

import Data.List
import Data.Function

import MJI
import Monad
import MonadFish
import MonadJoin

instance MonadJoin [] where
    returnJoin x = [x]

    join = concat
