{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}

module MonadJoin where

import Control.Applicative

class Applicative m => MonadJoin m where
    returnJoin :: a -> m a

    join :: m (m a) -> m a
