{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module Set4 where
import MCPrelude

--------------------------------------------------------------------------------
-- Generalizing State and Maybe

--genTwo :: Gen a -> (a -> Gen b) -> Gen b
--link :: Maybe a -> (a -> Maybe b) -> Maybe b

--generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
--yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c


f :: m a -> (a -> m b) -> m b
f = undefined

g :: (a -> b -> c) -> m a -> m b -> m c
g = undefined

--------------------------------------------------------------------------------
-- A missed generalization

--------------------------------------------------------------------------------
-- Formalizing the pattern

--------------------------------------------------------------------------------
-- Creating instances

--------------------------------------------------------------------------------
-- Revisiting other generic functions

--------------------------------------------------------------------------------
-- Using the abstraction
