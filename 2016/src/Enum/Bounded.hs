module Enum.Bounded where

-- | a `succ` that wraps 
succB :: (Bounded a, Enum a, Eq a) => a -> a 
succB en | en == maxBound = minBound
         | otherwise = succ en

-- | a `pred` that wraps
predB :: (Bounded a, Enum a, Eq a) => a -> a
predB en | en == minBound = maxBound
         | otherwise = pred en  