{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

class MapVar m v where
    put :: String -> v -> m v -> m v
    get :: String -> m v -> Maybe v

newtype MaybeMap a = MaybeMap (Maybe (String, a)) deriving(Show)

instance MapVar MaybeMap a where
    put s v _ = MaybeMap (Just (s,v))

    get s (MaybeMap Nothing) = Nothing
    get s (MaybeMap (Just (k,v)))
        | s == k             = Just v
        | otherwise          = Nothing

newtype ListMap a = ListMap [(String, a)] deriving(Show)

add :: ListMap a -> ListMap a -> ListMap a
add (ListMap x) (ListMap y) = ListMap (x ++ y)

instance MapVar ListMap a where
    put s v (ListMap []) = ListMap [(s, v)]
    put s v (ListMap ((s',v'):xs))
        | s' == s        = ListMap ((s,v):xs)
        | otherwise      = add (ListMap [(s',v')]) (put s v (ListMap xs))

    get s (ListMap []) = Nothing
    get s (ListMap ((s1,v):xs))
        | s1 == s      = Just v
        | otherwise    = get s (ListMap xs)

data TreeMap a = Leaf | Node (String,a) (TreeMap a) (TreeMap a) deriving(Show)

instance MapVar TreeMap a where
    put s v Leaf    = Node (s,v) Leaf Leaf
    put s v (Node (s',v') l r)
        | s == s'   = Node (s,v) l r
        | s < s'    = Node (s',v') (put s v l) r
        | otherwise = Node (s',v') l (put s v r)

    get s Leaf = Nothing
    get s (Node (s',v') l r)
        | s == s'   = Just v'
        | s < s'    = get s l
        | otherwise = get s r

getNotDefault :: (MapVar m v, MapVar m' v) => String -> m v -> m' v -> Maybe v
getNotDefault s m m' = case get s m of
                        Nothing -> get s m'
                        t -> t
