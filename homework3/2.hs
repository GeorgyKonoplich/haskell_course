module HW3 where

import Data.List
import Data.Maybe

getNext :: Int -> Int -> Maybe (Int, Int)
getNext n k = getNextImpl k
    where s = floor $ sqrt $ fromIntegral n
          getNextImpl m
              | m > s        = Nothing
              | mod n m == 0 = Just (m, div n m)
              | otherwise    = getNextImpl (m + 1)

factorize :: Int -> [Int]
factorize 1 = []
factorize n = factorizeImpl n 2
    where factorizeImpl m k = let r = getNext m k in if isNothing r
                                                     then [m]
                                                     else let (a, b) = fromJust r in a : (factorizeImpl b a)

factor :: [Int] -> [Int]
factor x = sort $ x >>= factorize
