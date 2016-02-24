{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}

module MonadFish where

import Control.Applicative

class Applicative m => MonadFish m where
    returnFish :: a -> m a

    (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

infixl 1 >=>
