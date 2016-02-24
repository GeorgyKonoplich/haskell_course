--Написать поиск n-ого числа Фибоначчи с использованием монады State
import Control.Monad.State.Strict 
import Control.Applicative

fibonacci n = evalState state_fib (0, 1, n)

state_fib :: State (Int, Int, Int) Int
state_fib = get >>= \(x1, x2, n) -> if n == 0
                                     then return x1
                                     else (put (x2, x1+x2, n-1) >> state_fib)

--тест
main = putStrLn $ show (fibonacci <$> [1..10])