{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module Set4 where
import MCPrelude
import Set2

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

-- (resolved in Set1.hs)

--------------------------------------------------------------------------------
-- Formalizing the pattern

class Monad m where
  bind :: m a -> (a -> m b) -> m b
  return :: a -> m a

--------------------------------------------------------------------------------
-- Creating instances

instance Monad Maybe where
  return x = Just x
  Just(x) `bind` f = f x
  Nothing `bind` _ = Nothing

instance Monad [] where
  return x = [x]
  xs `bind` f = concatMap f xs

instance Monad Gen where
  return x = Gen (\s -> (x, s))
  g `bind` f = Gen (\s -> let (r, s') = runGen g s in runGen (f r) s')

newtype Gen a = Gen {runGen :: Seed -> (a, Seed)}

evalGen :: Gen a -> Seed -> a
evalGen g s = fst $ runGen g s

--------------------------------------------------------------------------------
-- Revisiting other generic functions

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma =
    ma `bind` \a ->
    return $ f a

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb =
    ma `bind` \a ->
    mb `bind` \b ->
    return $ f a b

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f ma mb mc =
    ma `bind` \a ->
    mb `bind` \b ->
    mc `bind` \c ->
    return $ f a b c

sequence :: (Monad m) => [m a] -> m [a]
sequence [] = return []
sequence (m:ms) =
    m `bind` \x ->
    (sequence ms) `bind` \xs ->
    return (x:xs)

sequence2 :: (Monad m) => [m a] -> m [a]
sequence2 [] = return []
sequence2 (x:xs) = liftM2 (:) x (sequence2 xs)

sequence3 :: (Monad m) => [m a] -> m [a]
sequence3 [] = return []
sequence3 (m:ms) = return (:) `ap` m `ap` sequence3 ms

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip bind

join :: Monad m => m (m a) -> m a
join mma = mma `bind` id

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma =
    mf `bind` \f ->
    ma `bind` \a ->
    return $ f a

--------------------------------------------------------------------------------
-- Using the abstraction

-- runGen randGen $ mkSeed 1
-- (16807,Seed {unSeed = 16807})
randGen :: Gen Integer
randGen = Gen rand

-- runGen randLetter $ mkSeed 1
-- ('l',Seed {unSeed = 16807})
randLetter :: Gen Char
randLetter = liftM toLetter randGen

randEven :: Gen Integer
randEven = liftM (*2) randGen

randOdd :: Gen Integer
randOdd = liftM (+1) randEven

randTen :: Gen Integer
randTen = liftM (*10) randGen

-- runGen (generalPair randLetter randOdd) (mkSeed 1)
-- (('l',564950499),Seed {unSeed = 282475249})
generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair g1 g2 =
    g1 `bind` \r1 ->
    g2 `bind` \r2 ->
    return (r1, r2)

generalPair2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalPair2 f ga gb = liftM2 f ga gb

repRandom :: [Gen a] -> Gen [a]
repRandom gs = sequence3 gs
