import  Data.Monoid
import  Control.Applicative

newtype Writer w a = Writer { runWriter :: (a, w) } -- a is value, w is log

instance  Functor (Writer w) where
	fmap f a = Writer $ let (b, w) = runWriter a in (f b, w)  

instance (Monoid w) => Applicative (Writer w) where
	pure a = Writer (a, mempty)
	Writer (f, w) <*> some =  fmap f some

instance (Monoid w) => Monad (Writer w) where
    return a              = Writer (a, mempty)
    (Writer (x, v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')