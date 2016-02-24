import Data.Array.IO
import System.Environment
import Data.List
import System.IO

type CharSet = String

getIndex:: CharSet -> Char -> Int

getIndex xs x = head $ filter ((== x) . (xs !!)) [0..] 

tr :: CharSet -> Maybe CharSet -> String -> String

tr _inset Nothing xs = [x | x <-xs, (x `elem` _inset) == False] 
tr _inset (Just _outset) xs = [if x `elem` _inset then _outset !! (getIndex _inset x) else x| x <- xs]

repeating::CharSet->Int->CharSet

repeating xs n = xs ++ (replicate n (last xs)) 

main = do
	[t1, t2] <- getArgs
	str <- getLine
	if (t1 == "-d")
	then putStrLn $ tr t2 Nothing str
	else if length t1 > length t2 
		 then putStrLn $ tr t1 (Just (repeating t2 (length t1 - length t2))) str 
		 else putStrLn $ tr t1 (Just t2) str
	main