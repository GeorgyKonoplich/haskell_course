{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}

module MFITest where

import Data.List
import Data.Function

import MFI
import Monad
import MonadFish
import MonadJoin

instance MonadFish [] where
    returnFish x = [x]

    f >=> g = concat . map g . concat . map f . returnFish
