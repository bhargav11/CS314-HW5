module HW5 where

data Tree a = Tip | Bin (Tree a) a (Tree a) deriving (Show, Eq)
data Rose a = Node a [Rose a] deriving (Show, Eq)

fromTree :: Tree a -> [a]
fromTree Tip = []
fromTree (Bin l x r) = (fromTree l) ++ [x] ++ (fromTree r)


trunc :: Int -> Tree a -> Tree a
trunc n _ | n<1 = Tip
trunc n Tip = Tip
trunc n (Bin l x r) = (Bin (trunc (n-1) l) x (trunc (n-1) r) )

symmetric :: (Eq a) => Tree a -> Bool
symmetric (Bin l x r) = not (l == r)

sumRose :: (Num a) => Rose a -> a
sumRose (Node x cs) = x + sum (map sumRose cs)

maximumRose :: (Ord a) => Rose a -> a
maximumRose (Node x cs) 
                      | (maximum (map maximumRose cs) == x) = x
                      | (maximum (map maximumRose cs) > x) = maximum (map maximumRose cs)
                      | (maximum (map maximumRose cs) < x) = x

sizeRose :: Rose a -> Int
sizeRose (Node x cs) = 1 + length(cs)

fanout :: Rose a -> Int
fanout (Node x cs) = length(cs)
--is this the right way to approach this problem?^

toRoses :: Tree a -> [Rose a]
toRoses (Bin l x r) = [(Node x (toRoses (Bin l x r)))]
--How to covert from Rose a to [Rose a]?
--I am unsure that this is correct since there might be infinite recursion

fromRoses :: [Rose a] -> Tree a
fromRoses [(Node x cs)] = (Bin (fromRoses cs) x Tip )
--Only added one fromTree since two might be repetitive
--set the right side to Tip 

 


