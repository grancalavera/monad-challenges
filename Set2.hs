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
        Just h -> case divMay (fromIntegral mx) (fromIntegral h) of
          Nothing -> Nothing
          Just q' -> Just (q')

--------------------------------------------------------------------------------
-- Generalizing chains of failures

--------------------------------------------------------------------------------
-- Chaining variations

--------------------------------------------------------------------------------
-- Tailprod
