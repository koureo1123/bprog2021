{-# LANGUAGE NPlusKPatterns #-}
module SkTree where

data Tree
    = Leaf
    | Branch Tree Tree
    deriving (Eq, Show)

size :: Tree -> Int
size Leaf = 0
size (Branch s t) = 1 + size s + size t

sample1 :: Tree
sample1 = Leaf

sample2 :: Tree
sample2 = Branch (Branch sample1 sample1) sample1

sample3 :: Tree
sample3 = Branch sample2 (Branch sample1 sample2)

mktrees :: Int -> [Tree]
mktrees 0 = [Leaf]
mktrees (n+1) = undefined 

treespair :: (Int, Int) -> ([Tree], [Tree])
treespair (p,q) = (mktrees p, mktrees q)

trees :: ([Tree], [Tree]) -> [Tree]
trees (ss, ts) = [Branch s t | s <- ss, t <- ts] 

splits :: Int -> [(Int, Int)]
splits 0 = [(0, 0)]
splits (n+1) = (0,n+1) : [(p+1, q) | (p,q) <- splits n 