mergeSort :: (Ord a) => [a] -> [a]
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] x = x
merge x [] = x
merge (x:xs) (y:ys) = if x > y then y:(merge (x:xs) ys) else x:(merge xs (y:ys)) 

mergeSort [] = []
mergeSort xs
	| length xs == 1 	= xs 
	| otherwise 		= merge (mergeSort[y | y <- (take (length xs `div` 2) xs)]) (mergeSort[y | y <- (drop (length xs `div` 2) xs)])

main = print([1] ++ mergeSort [3, 2, 1])
--quickSort :: (Ord a) => [a] -> [a]
--quickSort [] = []
--quickSort (x:xs) 
--   = quickSort [y | y <- xs, y <= x] ++ [x] ++ quickSort [y | y <- xs, y > x]

--main = print(quickSort [3, 2, 3])