import Data.Char
input = "aba3caa ba12"
dels :: String -> String
dels [] = []
dels (x:xs) 
	| isDigit x    		= x: dels xs
	| otherwise		    = ' ': dels xs 
sumList :: String -> Int
sumList a = sum(map read $ words a :: [Int])

main = print (sumList(dels input)) 
