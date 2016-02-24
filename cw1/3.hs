{-# ExistentialQuantification #-}
module CachedSumList(CachedSumList, indexOf, insertIntoHead, deleteFromHead, createList, lst) where 
data CachedSumList a = CachedSumList {lst :: [a]} 

createList :: (Num a) => [a] -> CachedSumList a
createList l = CachedSumList l

shift :: (Num a) => CachedSumList a -> CachedSumList a
shift (CachedSumList a) = CachedSumList b where
	(xs, ys) = splitAt ((length a) - 1) a
	b = ys ++ xs


insertIntoHead :: (Num a) => a -> CachedSumList a -> CachedSumList a
insertIntoHead x (CachedSumList l) = CachedSumList (x : l) 

deleteFromHead :: (Num a) => CachedSumList a -> CachedSumList a
deleteFromHead (CachedSumList (x:xs) ) = CachedSumList xs
deleteFromHead (CachedSumList []) = error "delete from empty list" 


indexOf :: (Num a) => Int -> CachedSumList a -> a
indexOf ind (CachedSumList l) = b where
		(xs, ys) = splitAt ind l
		b = last xs
		
