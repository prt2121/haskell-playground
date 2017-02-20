module Main where

class Functar f where
  fmp :: (a -> b) -> f a -> f b

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woo"]

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f b -> f Char
liftedReplace = fmap replaceWithP

-- Heavy Lifting
ex1 = fmap (+1) $ read "[1]" :: [Int]

ex2 = (fmap . fmap) (++ "world") (Just ["hi, ", "hello, "])

ex3 = fmap (*2) (\x -> x - 2)

ex4 = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

ex5 :: IO Integer
ex5 = let ioi = readIO "1" :: IO Integer
          changed = fmap read (fmap ("123" ++) (fmap show ioi))
      in fmap (*3) changed


-- (a -> b) -> f a -> f b -> (b -> c) -> f b -> f c

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- fmap :: Functor f1 => (x -> y) -> f x -> f y

-- (x -> y) -> (f1 x -> f1 y) -> (f1 a -> f1 b) -> (f f1 a -> f f1 b)


main :: IO ()
main = do
  putStrLn "hello world"
