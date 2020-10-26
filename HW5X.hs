{-# LANGUAGE StandaloneDeriving, UndecidableInstances #-}

module HW5X where

import HW5

data GRose c a = GNode a (c (GRose c a))
data TwoThree a = Tip | Two (Tree a) a (Tree a) | Three (Tree a) a (Tree a) a (Tree a) deriving (Show, Eq)

deriving instance (Show a, Show (c (GRose c a))) => Show (GRose c a)
deriving instance (Eq a, Eq (c (GRose c a))) => Eq (GRose c a)

genRose :: Rose a -> GRose [] a
genRose (Node a c) = GNode a [(genRose (Node a c))]

ungenRose :: GRose [] a -> Rose a
ungenRose (GNode a cs) = Node a [(ungenRose (GNode a cs))]

sum23 :: (Num a) => GRose TwoThree a -> a
--sum23 (GNode 0 cs) = 0
sum23 (GNode a cs) = a + sum23 (GNode (a-1) cs)
--Question about how to write the above base case 
