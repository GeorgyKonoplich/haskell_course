--Напишите функцию, которая считывает 5 строк из консоли, но если в какой-то момент считанная строка является подстрокой
--одной из считанных строк, необходимо завершить считывание и вывести сообщение о том что возникла ошибка. 
--Иначе надо вывести, что все ок.

{-# LANGUAGE FlexibleContexts #-}
import Data.List

loop :: [String] -> IO ()
loop arr = do
	if ((length arr) == 5)
	  then
	  	putStrLn ("--All OK--")
	  else do
	  	line <- getLine
	  	step line arr

step :: String -> [String] -> IO ()
step xs arr = do
	let bl = map (isInfixOf xs) arr 
	if (elem True bl)
	  then do
	  	putStrLn ("--Error: substring was found--")
	  else do
	  	loop (arr ++ [xs])

main :: IO()
main = do
 loop []
  