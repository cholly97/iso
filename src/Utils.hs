module Utils where

import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Data.Function

data CompBy a b = CompBy {compFunc :: (a -> b), value :: a}

compByFunc
  :: Ord b
  => ((CompBy a b -> CompBy a b -> Ordering) -> t (CompBy a b) -> CompBy a b)
  -> t (CompBy a b)
  -> a
compByFunc = eval <-< on compare >- compFunc <*> value >>-> value

------------------------------ Combinators -------------------------------------

flap :: Applicative f => f (a -> b) -> a -> f b
flap fab a = fab <*> pure a

infixl 4 <<$>>
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

ffmap :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
ffmap = (<<$>>)

infixl 0 -:
(-:) = flip ($)

eval = (-:)

infixl 1 >-
(>-) :: (a -> b) -> a -> b
f >- x = f x
infixl 1 -<
(-<) :: a -> (a -> b) -> b
x -< f = f x


infixl 1 -<>
(-<>) :: b -> (a -> b -> c) -> a -> c
(-<>) = flip flip
infixl 1 <>-
(<>-) :: (a -> b -> c) -> b -> a -> c
(<>-) = flip


infixl 1 >->
(>->) :: (a -> b) -> (b -> c) -> a -> c
(>->) = (>>>)
infixl 1 <-<
(<-<) :: (b -> c) -> (a -> b) -> a -> c
(<-<) = (<<<)


infixl 1 >-<>
(>-<>) :: (b -> x) -> (a -> x -> c) -> a -> b -> c
f >-<> g = (f >->) <-< g
infixl 1 <>-<
(<>-<) :: (a -> x -> c) -> (b -> x) -> a -> b -> c
f <>-< g = f >-> (<-< g)


infixl 1 >>->
(>>->) :: (a -> b -> x) -> (x -> c) -> a -> b -> c
f >>-> g = f >-> (>-> g)
infixl 1 <-<<
(<-<<) :: (x -> c) -> (a -> b -> x) -> a -> b -> c
f <-<< g = (f <-<) <-< g


infixl 1 >=<>
(>=<>) :: (b -> x) -> (a -> x -> b -> c) -> a -> b -> c
f >=<> g = (f >>=) <-< g
infixl 1 <>=<
(<>=<) :: (a -> x -> b -> c) -> (b -> x) -> a -> b -> c
f <>=< g = f >-> (=<< g)


infixl 1 >>=>
(>>=>) :: (a -> b -> x) -> (x -> b -> c) -> a -> b -> c
f >>=> g = f >-> (>>= g)
infixl 1 <=<<
(<=<<) :: (x -> b -> c) -> (a -> b -> x) -> a -> b -> c
f <=<< g = (f =<<) <-< g
