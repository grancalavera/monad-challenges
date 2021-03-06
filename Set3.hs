{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module Set3 where
import MCPrelude

--------------------------------------------------------------------------------
-- Generating combinations

allPairs :: [a] -> [b] -> [(a, b)]
allPairs (x:xs) ys = pairWith x ys ++ allPairs xs ys
allPairs _ _ = []

pairWith x (y:ys) = (x, y) : pairWith x ys
pairWith _ _ = []

allPairs' :: [a] -> [b] -> [(a, b)]
allPairs' (x:xs) ys = map ((,) x) ys ++ allPairs' xs ys
allPairs' _ _ = []

--------------------------------------------------------------------------------
-- Poker hands

data Card = Card Int String

instance Show Card where
  show (Card r s) = show r ++ s

allCards :: [Int] -> [String] -> [Card]
allCards rs ss = map (uncurry makeCard) $ allPairs rs ss

makeCard r s = Card r s

--------------------------------------------------------------------------------
-- Generalizing pairs and cards

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs f (x:xs) ys = combWith f x ys ++ allCombs f xs ys
allCombs _ _ _ = []

combWith f x (y:ys) = f x y : combWith f x ys
combWith _ _ _ = []

allCards' :: [Int] -> [String] -> [Card]
allCards' rs ss = allCombs makeCard rs ss

allCombs' :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs' f (x:xs) ys = map (f x) ys ++ allCombs' f xs ys
allCombs' _ _ _ = []

--------------------------------------------------------------------------------
-- Combinations of three things

a = [1, 2]
b = [3, 4]
c = [5, 6]

r =
  [
    (1,3,5)
  , (1,3,6)
  , (1,4,5)
  , (1,4,6)
  , (2,3,5)
  , (2,3,6)
  , (2,4,5)
  , (2,4,6)
  ]

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f (x:xs) ys zs = step1 f x ys zs ++ allCombs3 f xs ys zs
allCombs3 _ _ _ _ = []

step1 f x (y:ys) zs = step2 f x y zs ++ step1 f x ys zs
step1 _ _ _ _ = []

step2 f x y (z:zs) = f x y z : step2 f x y zs
step2 _ _ _ _ = []

--------------------------------------------------------------------------------
-- Combinations of more things
-- The Cheat
-- https://github.com/spearsem/learnmeahaskell/blob/master/monad_challenge_solutions/Set3.hs#L41

combStep :: [a -> b] -> [a] -> [b]
combStep [] _ = []
combStep _ [] = []
combStep (f:fs) xs = map f xs ++ (combStep fs xs)

allCombs3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3' f xs ys zs = (map f xs) `combStep` ys `combStep` zs
