{-# OPTIONS_GHC -Wall #-}
-- homework 4
-- http://www.cis.upenn.edu/~cis194/spring13/hw/04-higher-order.pdf
module HigherOrder04 where

-- Exercise 1: Wholemeal programming
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (\x acc -> (x - 2) * acc) 1 . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n  | even n = n + fun2 (n `div` 2)
        | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (1 /=) . iterate f
            where f x | even x    = x `div` 2
                      | otherwise = 3 * x + 1

data Tree a = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

-- generate a balanced binary tree
foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
            where
                insert x Leaf = Node 0 (Leaf) x (Leaf)
                insert x (Node n l v r)
                    | hl < hr   = Node n (insert x l) v r
                    | hl > hr   = Node n l v t
                    | otherwise = Node (h+1) l v t
                    where   hl = treeHeight l
                            hr = treeHeight r
                            t  = insert x r
                            h  = treeHeight t

treeHeight :: Tree a -> Integer
treeHeight Leaf = -1
treeHeight (Node depth _ _ _) = depth