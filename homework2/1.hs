data CachedSumList a = CachedSumList {sumall :: a, lst :: [a]} 

createList :: (Num a) => [a] -> CachedSumList a
createList l = CachedSumList (sum l) l

instance (Ord a) => Ord (CachedSumList a) 
  where
	(CachedSumList a t) <= (CachedSumList b e) = a <= b

instance (Eq a) => Eq (CachedSumList a) 
  where
	(CachedSumList a t) == (CachedSumList b e) = a == b

instance (Num a ) => Monoid (CachedSumList a) where
	mempty = createList []
	mappend (CachedSumList a t ) (CachedSumList b e) = CachedSumList (a + b) (t ++ e)


insertIntoHead :: (Num a) => a -> CachedSumList a -> CachedSumList a
insertIntoHead x (CachedSumList sum1 l) = CachedSumList (sum1 + x) (x : l) 

deleteFromHead :: (Num a) => CachedSumList a -> CachedSumList a
deleteFromHead (CachedSumList sum1 (x:xs) ) = CachedSumList (sum1 - x) xs
deleteFromHead (CachedSumList sum1 []) = error "delete from empty list" 

deleteIndex :: (Num a) => Int -> CachedSumList a -> CachedSumList a
deleteIndex ind (CachedSumList sum1 l) = CachedSumList sum2 l2 
  where
		(xs, ys) = splitAt ind l
		l2 = xs ++ (tail ys)
		sum2 = sum1 - (head ys)

indexOf :: (Num a) => Int -> CachedSumList a -> a
indexOf ind (CachedSumList sum1 l) = b where
		(xs, ys) = splitAt ind l
		b = last xs
		
