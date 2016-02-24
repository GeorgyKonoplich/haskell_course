data BinTree a = EmptyTree | Node a (BinTree a) (BinTree a)

singleton :: a -> BinTree a
singleton x = Node x EmptyTree EmptyTree

insert :: (Ord a) => a -> BinTree a -> BinTree a
insert x EmptyTree = singleton x
insert x (Node a l r)
	| x == a = Node x l r
	| x < a = Node a (insert x l) r
	| x > a =  Node a l (insert x r)

find :: (Ord a) => a -> BinTree a -> Bool
find x EmptyTree = False
find x (Node a l r)
	| x == a = True
	| x > a = find x r
	| x < a = find x l

fromList :: (Ord a) => [a] -> BinTree a
fromList xs = insertFromList xs EmptyTree

insertFromList :: (Ord a) => [a] -> BinTree a -> BinTree a
insertFromList [] binTree = binTree
insertFromList (x:xs) binTree = insertFromList xs (insert x binTree)

delete :: (Ord a) => a -> BinTree a -> BinTree a
delete x EmptyTree = EmptyTree
delete x (Node a l r)
	| x > a = Node a l (delete x r)
	| x < a = Node a (delete x l) r
	| x == a = order l r
	where 
		  order :: (Ord a) => BinTree a -> BinTree a -> BinTree a
		  order EmptyTree r = r
		  order l EmptyTree = l
		  order l r = Node a2 l r2
		  		where 
		  			  a2 = rightMin r
		  			  r2 = delete a2 r

		  			  rightMin :: (Ord a) => BinTree a -> a
		  			  rightMin (Node a _ EmptyTree) = a
		  		  	  rightMin (Node _ l _) = rightMin l

printTree :: Show a => BinTree a -> IO ()
printTree = mapM_ putStrLn . treeIndent
  where
    treeIndent EmptyTree          = ["-- /-"]
    treeIndent (Node k lb rb) =
      ["--" ++ show k] ++
      map ("  |" ++) ls ++
      ("  `" ++ r) : map ("   " ++) rs
      where
        (r:rs) = treeIndent rb
        ls     = treeIndent lb		  		  	  




  
