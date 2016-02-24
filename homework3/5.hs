import Control.Monad


class I a where
    measure :: a -> Int -> Int

class J a where
    getParameter :: a -> Int

doMeasure :: (I a, J a) => a -> Int
doMeasure obj = measure obj (getParameter obj)

newtype P = P { getP :: Int}
		
instance I P where
	measure a b = b * (getP a)

instance J P where
	getParameter a = getP a