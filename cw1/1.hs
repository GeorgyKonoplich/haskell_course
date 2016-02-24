filtr :: [String] -> Char -> [String]
filtr [] b = []
filtr (x:xs) b  
	| x `elem` xs	= filtr xs b 
	| x !! 0 == b 	= x:(filtr xs b) 
	| otherwise 	= filtr xs b 
main = print (filtr ["aasds", "sda", "sda"] 's')
