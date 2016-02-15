module Hw06Lazy where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- the infinite list of all Fibonacci numbers
fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = 0 : 1 : ls
        where ls = zipWith (+) fibs2 (drop 1 fibs2)

-- Exercise 3
-- Define a data type of polymorphic streams, Stream
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons n s) = n : streamToList s

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

-- Exercise 4
-- generates a stream containing infinitely many copies of the
-- given element.
streamRepeat :: a -> Stream a
streamRepeat n = Cons n $ streamRepeat n

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons n s) = Cons (f n) $ streamMap f s

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f i = Cons i $ streamFromSeed f $ f i

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

--ruler :: Stream Integer
-- 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4,...
-- todo :P

main :: IO ()
main = do
  putStrLn "hello world"
