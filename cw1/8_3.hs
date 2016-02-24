{-
	Структура данных : циклический список. Операции : shift(сдвинуть список на 1 вправо), !!!(значение списка по индексу)
-}
data RingList a = RingList { list :: [a] }

shift :: (Num a) => RingList a -> RingList a
shift (RingList a) = RingList b where
	(xs, ys) = splitAt ((length a) - 1) a
	b = ys ++ xs

(!!!) :: (Num a) => RingList a -> Int -> a
(!!!) xs x 
	| x < 0 || x >= (length (list xs)) = error "array index out of bounds exception"
	| otherwise = last (take x (list xs))

main = do
	let a = RingList [1, 2, 3, 4]
	let b = shift a -- [4, 1, 2, 3]
	let c = shift b -- [3, 4, 1, 2]
	let d = shift c -- [2, 3, 4, 1]
	let e = shift d -- [1, 2, 3, 4]
	print (list c)
	print ((!!!) c 2)

