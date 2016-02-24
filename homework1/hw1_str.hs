stringSum :: String -> Int

stringSum = sum . (map read2) . words
	where 
		read2 ('+':xs)  = read xs :: Int
		read2 xs = read xs :: Int