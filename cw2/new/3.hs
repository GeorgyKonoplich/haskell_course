--Напишите функцию, которую считывает строки из консоли, пока не встретит строку полностью из цифр.
--И если цифры в последней строке четные, то функция должна вывести ошибку, иначе должна вывести все считанные строки. 

{-# LANGUAGE FlexibleContexts #-}
import Data.List
import Data.Char

loop :: [String] -> IO ()
loop arr = do
	  	line <- getLine
	  	step line arr

step :: String -> [String] -> IO ()
step line arr = do
		if (prov line)
		then if (check line) 
			then putStrLn ("Error")
			else putStrLn $ show (arr ++ [line])
		else loop (arr ++ [line])

check :: [Char] -> Bool
check [] = True
check (x:xs) = if (odd (digitToInt x))
				then
					False
				else
					check xs 
				

prov :: [Char]->Bool
prov [] = True
prov (x:xs) = if (x `elem` ['0'..'9'])
				then
					prov xs
				else
					False  

main = do
 loop []
  