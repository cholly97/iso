module Utils.Comparison where

import           Utils.Combinators

import           Data.Function

data CompBy a b = CompBy {compFunc :: a -> b, value :: a}

compByFunc
  :: Ord b
  => ((CompBy a b -> CompBy a b -> Ordering) -> t (CompBy a b) -> CompBy a b)
  -> t (CompBy a b)
  -> a
compByFunc = eval <-< on compare -< compFunc <*> value >>--> value
