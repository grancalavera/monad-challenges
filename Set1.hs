{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module Set1 where
import MCPrelude

--------------------------------------------------------------------------------
-- Random number generation

fiveRands :: [ Integer ]
fiveRands = rands (mkSeed 1) 5

rands _ 0 = []
rands s n = r : rands s' (n - 1)
  where
    (r, s') = rand s

--------------------------------------------------------------------------------
-- Random character generation

randLetter :: Gen Char
randLetter = generalA toLetter rand

randString _ 0 = []
randString s n = r : randString s' (n - 1)
  where
    (r, s') = randLetter s

randString3 :: String
randString3 = randString (mkSeed 1) 3

--------------------------------------------------------------------------------
-- More generators

type Gen a = (Seed -> (a, Seed))

randEven :: Gen Integer
randEven = generalA (*2) rand

randOdd :: Gen Integer
randOdd = generalA (+1) randEven

randTen :: Gen Integer
randTen = generalA (*10) rand

-- generalA :: (a -> b) -> (Seed -> (a, Seed)) -> Seed -> (b, Seed))
-- generalA :: (a -> b) -> Gen a -> Gen b
generalA :: (a -> b) -> Gen a -> Gen b
generalA f g s = (f r, s')
  where
    (r, s') = g s

--------------------------------------------------------------------------------
-- Generalizing Random Pairs

randPair :: Gen (Char, Integer)
randPair = generalPair randLetter rand

randPair2 :: Gen (Char, Integer)
randPair2 = generalPair randLetter rand

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair g1 g2 s = ((x, y), s'')
  where
    (x, s') = g1 s
    (y, s'') = g2 s'

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 g1 g2 = generalB f g1 g2
  where f x y = (x, y)

-- generalPair2 randOdd randTen (mkSeed 1)

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f g1 g2 s = (f x y, s'')
  where
    (x, s') = g1 s
    (y, s'') = g2 s'

--------------------------------------------------------------------------------
-- Generalizing Lists of Generators
-- Gen a = (Seed -> (a, Seed))
-- Gen [a] = (Seed -> ([a], Seed))

repRandom :: [Gen a] -> Gen [a]
repRandom [] s = ([], s)
repRandom (g:gs) s = (r:rs, s'')
  where
    (r, s') = g s
    (rs, s'') = repRandom gs s'

fiveRands' = repRandom (replicate 5 rand) (mkSeed 1)

--------------------------------------------------------------------------------
-- Threading the random number state

-- genTwo :: (Seed -> (a, Seed)) -> (a -> (Seed -> (b, Seed))) -> Seed -> (b, Seed)
genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo g f s = f r s'
  where
    (r, s') = g s

-- ??
-- genTwo rand (\r -> randLetter) (mkSeed 1)

-- actually this is better
-- genTwo rand (\r -> mkGen r) (mkSeed 1)

-- mkGen :: a -> Seed -> (a, Seed)
mkGen :: a -> Gen a
mkGen x s = (x, s)

--------------------------------------------------------------------------------
-- flashback from Set4

generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB2 f g1 g2 = genTwo g1 (\r1 ->
                      genTwo g2 (\r2 ->
                        mkGen (f r1 r2)))

repRandom' :: [Gen a] -> Gen [a]
repRandom' [] = mkGen []
repRandom' (g:gs) = genTwo g (\r ->
                      genTwo (repRandom' gs) (\rs ->
                          mkGen (r:rs)))

fiveRands'' = repRandom' (replicate 5 rand) (mkSeed 1)
