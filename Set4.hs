{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module Set4 where
import MCPrelude
import Set2 (
    Maybe(Just, Nothing)
  , headMay
  , tailMay
  , lookupMay
  , divMay
  )

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

--------------------------------------------------------------------------------
-- Random Generators

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

fiveRands :: [Integer]
fiveRands = evalGen (repRandom $ replicate 5 randGen) (mkSeed 1)

--------------------------------------------------------------------------------
-- The Maybe type

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay (x:xs) = Just (foldl max x xs)

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay (x:xs) = Just (foldl min x xs)

queryGreek :: GreekData -> String -> Maybe Double
queryGreek d q =
    lookupMay q d   `bind` \xs  ->
    tailMay xs      `bind` \xs' ->
    maximumMay xs'  `bind` \mx  ->
    headMay xs      `bind` \h   ->
    divMay (fromIntegral mx) (fromIntegral h)

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries ss e1 e2 = liftM2 (+) (lookupMay e1 ss) (lookupMay e2 ss)

tailFMay :: ([a] -> b) -> [a] -> Maybe b
tailFMay f xs = liftM f (tailMay xs)

tailProd :: Num a => [a] -> Maybe a
tailProd = tailFMay product

tailSum :: Num a => [a] -> Maybe a
tailSum = tailFMay sum

tailMax :: Ord a => [a] -> Maybe a
tailMax = join . tailFMay maximumMay

tailMin :: Ord a => [a] -> Maybe a
tailMin = join . tailFMay minimumMay

