module State where

import Control.Applicative

newtype State s r = State { function :: s -> (s, r) }

instance Applicative (State s) where
    pure a = State (\s -> (s, a))

    f <*> x = State (\s -> let (a, b) = function x s in let (c, d) = function f a in (c, d b))

instance Monad (State s) where
    return = pure

    x >>= f = State (\s -> let (a, b) = function x s in function (f b) a)
