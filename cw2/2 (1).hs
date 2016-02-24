--Реализуйте функции length и map, используя функцию traverse

{-# LANGUAGE FlexibleContexts #-}
import Data.List
import Control.Monad.Identity
import Data.Traversable

mapTr :: (a -> b) -> [a] -> [b]
mapTr f a1 = runIdentity $ traverse (\x -> Identity $ f x) a1

lengthTr :: [a] -> Int
lengthTr x = sum $ traverse (const (+1)) x 0


--Функция для тестирования
isOne :: Int -> Bool
isOne x = x == 1

main :: IO()
main = do
	putStrLn (show(mapTr (+1) [1,2,3]))
	putStrLn (show(mapTr (isOne) [1,1,1,2,3,4,5,1]))
	putStrLn (show(lengthTr [1,2,3,4,5,6,7]))