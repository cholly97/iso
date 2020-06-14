module Utils.FuzzyFloat where

newtype FuzzyFloat = FuzzyFloat {getFloat :: Float}

-- Float but with fuzzy equality
-- This means that equality/<= is not transitive!

instance Eq FuzzyFloat where
  x == y = abs (getFloat x - getFloat y) < 0.001

instance Ord FuzzyFloat where
  compare x y | x == y    = EQ
              | otherwise = compare (getFloat x) (getFloat y)
