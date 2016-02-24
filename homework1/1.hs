tests = ["1", "1 2 3", " 1", "1 ", "\t1\t", "\t12345\t", "010 020 030", 
         " 123 456 789 ", "-1", "-1 -2 -3", "\t-12345\t", " -123 -456 -789 ", 
         "\n1\t\n3   555  -1\n\n\n-5", "123\t\n\t\n\t\n321 -4 -40"]

sumList :: String -> Int
sumList a = sum(map read $ words a :: [Int])

main = print [sumList (tests !! x)| x <- [0..length tests - 1]]  


--sumList :: String -> Int
--sumList a = sum(map read $ words a :: [Int])
--for :: [String] -> [Int]
--for [] = []
--for (x:xs) = sumList x : for xs
--main = print (for tests)
