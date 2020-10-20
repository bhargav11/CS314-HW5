module HW5 where

data Tree a = Tip | Bin (Tree a) a (Tree a) deriving (Show, Eq)
data Rose a = Node a [Rose a] deriving (Show, Eq)

depthFirstInOrder :: Tree a -> [a]
depthFirstInOrder (Bin l v r) = (depthFirstInOrder l) ++ [v] ++ (depthFirstInOrder r)

fromTree :: Tree a -> [a]
fromTree (Bin l x r) = ( depthFirstInOrder (Bin l x r) )

--trunc :: Int -> Tree a -> Tree a

