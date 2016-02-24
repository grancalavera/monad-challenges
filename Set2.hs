{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module Set2 where
import MCPrelude

--------------------------------------------------------------------------------
-- The Maybe type

data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just x) = "Just " ++ show x

--------------------------------------------------------------------------------
-- Build a library of things that can fail

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (x:[]) = Nothing
tailMay (x:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay x ((y, z):yzs) = if x == y then Just z else lookupMay x yzs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay x y = Just (x / y)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay (x:xs) = mx (maximumMay xs)
  where
    mx Nothing = Just x
    mx (Just y) = if x > y then Just x else Just y

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay (x:xs) = case minimumMay xs of Nothing -> Just x
                                          (Just y) -> if x < y then Just x else Just y

--------------------------------------------------------------------------------
-- Chains of failing computations

queryGreek :: GreekData -> String -> Maybe Double
queryGreek d q = case lookupMay q d of
  Nothing -> Nothing
  Just xs -> case tailMay xs of
    Nothing -> Nothing
    Just xs' -> case maximumMay xs' of
      Nothing -> Nothing
      Just mx -> case headMay xs of
        Nothing -> Nothing
        Just h -> divMay (fromIntegral mx) (fromIntegral h)

--------------------------------------------------------------------------------
-- Generalizing chains of failures

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain _ Nothing = Nothing
chain f (Just x) = f x

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = flip chain

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 d q =
  lookupMay q d `link`
    (\xs ->
      tailMay xs `link`
        (\xs' ->          maximumMay xs' `link`
            (\mx ->
              headMay xs `link`
                (\h -> divMay (fromIntegral mx) (fromIntegral h) `link`
                  Just))))

--------------------------------------------------------------------------------
-- Chaining variations

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries ss e1 e2 =
  lookupMay e1 ss `link`
    (\s1 -> lookupMay e2 ss `link`
      (\s2 -> mkMaybe (s1 + s2)))

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink f ma mb = ma `link` (\x -> mb `link` (\y -> mkMaybe (f x y)))

addSalaries' :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries' ss e1 e2 = yLink (+) (lookupMay e1 ss) (lookupMay e2 ss)

mkMaybe :: a -> Maybe a
mkMaybe x = Just x

--------------------------------------------------------------------------------
-- Tailprod

tailProd :: Num a => [a] -> Maybe a
tailProd xs = tailMay xs `link` (\t -> mkMaybe (product t))

tailSum :: Num a => [a] -> Maybe a
tailSum xs = tailMay xs `link` (\t -> mkMaybe (sum t))

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe f ma = ma `link` (\x -> mkMaybe (f x))

tailProd' :: Num a => [a] -> Maybe a
tailProd' xs = transMaybe product (tailMay xs)

tailSum' :: Num a => [a] -> Maybe a
tailSum' xs = transMaybe sum (tailMay xs)

tailMax :: Ord a => [a] -> Maybe (Maybe a)
tailMax xs = tailMay xs `link` (\t -> mkMaybe $ maximumMay t)

tailMin :: Ord a => [a] -> Maybe (Maybe a)
tailMin xs = tailMay xs `link` (\t -> mkMaybe $ minimumMay t)

combine :: Maybe (Maybe a) -> Maybe a
combine mma = mma `link` id

tailMax' :: Ord a => [a] -> Maybe a
tailMax' xs = combine (tailMax xs)

tailMin' :: Ord a => [a] -> Maybe a
tailMin' xs = combine (tailMin xs)
