{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module Set3 where
import MCPrelude

--------------------------------------------------------------------------------
-- Generating combinations

allPairs :: [a] -> [b] -> [(a, b)]
allPairs _ [] = []
allPairs [] _ = []
allPairs (x:xs) ys = pairWith x ys ++ allPairs xs ys

pairWith x [] = []
pairWith x (y:ys) = (x, y) : pairWith x ys

--------------------------------------------------------------------------------
--Poker hands

--------------------------------------------------------------------------------
--Generalizing pairs and cards

--------------------------------------------------------------------------------
--Combinations of three things

--------------------------------------------------------------------------------
--Combinations of more things
