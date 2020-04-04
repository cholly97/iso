module Utils where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Function
import           Data.Functor.Contravariant

nothingIf :: (a -> Bool) -> a -> Maybe a
nothingIf f a = if f a then Nothing else Just a

-------------------------------- Comparison ------------------------------------

data CompBy a b = CompBy {compFunc :: a -> b, value :: a}

compByFunc
  :: Ord b
  => ((CompBy a b -> CompBy a b -> Ordering) -> t (CompBy a b) -> CompBy a b)
  -> t (CompBy a b)
  -> a
compByFunc = eval <-< on compare >- compFunc <*> value >>--> value

-------------------------------- Combinators -----------------------------------
infixl 0 >>>
(>>>) :: (Monad m0, Monad m1) => m1 (m0 a) -> m1 (m0 b) -> m1 (m0 b)
(>>>) = liftM2 (>>)
infixl 0 >>>>
(>>>>)
  :: (Monad m0, Monad m1, Monad m2)
  => m2 (m1 (m0 a))
  -> m2 (m1 (m0 b))
  -> m2 (m1 (m0 b))
(>>>>) = liftM2 (>>>)

infixl 0 >>>=
(>>>=) :: (Monad a, Monad m) => a (m b) -> a (b -> m c) -> a (m c)
(>>>=) = liftM2 (>>=)

pamf :: Functor f => f a -> (a -> b) -> f b
pamf = flip fmap

flap :: Functor f => f (a -> b) -> a -> f b
flap fab a = fmap ($ a) fab

palf :: Functor f => a -> f (a -> b) -> f b
palf = flip flap

infixl 0 -:
(-:) = flip ($)
eval = (-:)


-- generalized $ (2 types)
infixl 1 -<
(-<) :: a -> (a -> b) -> b
(-<) = flip (>-)
infixl 1 >-
(>-) :: (a -> b) -> a -> b
(>-) = id


-- generalized flip (3 types)
infixl 1 -<>
(-<>) :: Functor a => b -> a (b -> c) -> a c
(-<>) = flip (<>-)
infixl 1 <>-
(<>-) :: Functor a => a (b -> c) -> b -> a c
(<>-) = flap

-- generalized . (3 types)
infixl 1 >->
(>->) :: Functor a => a b -> (b -> c) -> a c
(>->) = flip (<-<)
infixl 1 <-<
(<-<) :: Functor a => (b -> c) -> a b -> a c
(<-<) = fmap
infixl 1 >~>
(>~>) :: Contravariant c => (a -> b) -> c b -> c a
(>~>) = contramap
infixl 1 <~<
(<~<) :: Contravariant c => c b -> (a -> b) -> c a
(<~<) = flip (>~>)
-- some more remixes
infixl 1 <<-
(<<-) :: (b -> c) -> a -> (a -> b) -> c
(<<-) = flip . fmap -- contramap eval
infixl 1 ->>
(->>) :: a -> (b -> c) -> (a -> b) -> c
(->>) = flip (<<-)
infixl 1 >>-
(>>-) :: (a -> b) -> a -> (b -> c) -> c
(>>-) = fmap eval -- fix (flip eval .)
infixl 1 -<<
(-<<) :: a -> (a -> b) -> (b -> c) -> c
(-<<) = flip (>>-)


-- generalized (.).flip(.) (4 types)
-- (b -> x) -> (a -> x -> c) -> a -> b -> c
infixl 1 >--<>
(>--<>) :: (Functor a, Functor b) => b x -> a (x -> c) -> a (b c)
(>--<>) = (>->) >-> (<-<)
infixl 1 >~-<>
(>~-<>) :: (Contravariant c, Functor a) => (b -> x) -> a (c x) -> a (c b)
(>~-<>) = (>~>) >-> (<-<)
infixl 1 <>--<
(<>--<) :: (Functor a, Functor b) => a (x -> c) -> b x -> a (b c)
(<>--<) = flip (>--<>)
infixl 1 <>-~<
(<>-~<) :: (Contravariant c, Functor a) => a (c x) -> (b -> x) -> a (c b)
(<>-~<) = flip (>~-<>)

-- generalized flip(.).(.) (4 types)
-- (b -> x) -> ((a -> x) -> c) -> (a -> b) -> c
infixl 1 <>~~<
(<>~~<) :: (Contravariant c, Contravariant x) => x b -> c (x a) -> c (a -> b)
(<>~~<) = (<~<) >-> (>~>)
infixl 1 <>~-<
(<>~-<) :: (Functor a, Contravariant c) => (b -> x) -> c (a x) -> c (a b)
(<>~-<) = (<-<) >-> (>~>)
infixl 1 >~~<>
(>~~<>) :: (Contravariant c, Contravariant x) => c (x a) -> x b -> c (a -> b)
(>~~<>) = flip (<>~~<)
infixl 1 >-~<>
(>-~<>) :: (Functor a, Contravariant c) => c (a x) -> (b -> x) -> c (a b)
(>-~<>) = flip (<>~-<)
-- special case where contravariant c is (flip (->) c')
infixl 1 <>-<
(<>-<) :: Functor a => (b -> x) -> (a x -> c) -> a b -> c
(<>-<) = (<-<) >-> (>->)
infixl 1 >-<>
(>-<>) :: Functor a => (a x -> c) -> (b -> x) -> a b -> c
(>-<>) = flip (<>-<)

-- generalized (.).(.) (4 types)
-- (x -> c) -> (a -> b -> x) -> a -> b -> c
infixl 1 <--<<
(<--<<) :: (Functor a, Functor b) => (x -> c) -> a (b x) -> a (b c)
(<--<<) = (<-<) >-> (<-<)
infixl 1 <~-<<
(<~-<<) :: (Contravariant c, Functor a) => c x -> a (b -> x) -> a (c b)
(<~-<<) = (<~<) >-> (<-<)
infixl 1 >>-->
(>>-->) :: (Functor a, Functor b) => a (b x) -> (x -> c) -> a (b c)
(>>-->) = flip (<--<<)
infixl 1 >>-~>
(>>-~>) :: (Contravariant c, Functor a) => a (b -> x) -> c x -> a (c b)
(>>-~>) = flip (<~-<<)

-- generalized flip(.).flip(.) (4 types)
-- (x -> a) -> ((x -> b) -> c) -> (a -> b) -> c
infixl 1 >>~->
(>>~->) :: (Functor x, Contravariant c) => x a -> c (x b) -> c (a -> b)
(>>~->) = (>->) >-> (>~>)
infixl 1 >>~~>
(>>~~>) :: (Contravariant b, Contravariant c) => (x -> a) -> c (b x) -> c (b a)
(>>~~>) = (>~>) >-> (>~>)
infixl 1 <-~<<
(<-~<<) :: (Functor x, Contravariant c) => c (x b) -> x a -> c (a -> b)
(<-~<<) = flip (>>~->)
infixl 1 <~~<<
(<~~<<) :: (Contravariant b, Contravariant c) => c (b x) -> (x -> a) -> c (b a)
(<~~<<) = flip (>>~~>)
-- special case where contravariant c is (flip (->) c')
infixl 1 >>->
(>>->) :: Functor x => x a -> (x b -> c) -> (a -> b) -> c
(>>->) = (>->) >-> (>->)
infixl 1 <-<<
(<-<<) :: Functor x => (x b -> c) -> x a -> (a -> b) -> c
(<-<<) = flip (>>->)

-- (b -> x) -> (a -> x -> b -> c) -> a -> b -> c
infixl 1 >=<>
(>=<>) :: (Monad b, Functor a) => b x -> a (x -> b c) -> a (b c)
(>=<>) = (>>=) >-> (<-<)
infixl 1 <>=<
(<>=<) :: (Monad b, Functor a) => a (x -> b c) -> b x -> a (b c)
(<>=<) = flip (>=<>)

-- (a -> x -> b) -> ((x -> b) -> c) -> (x -> a) -> c
infixl 1 <>≈<
(<>≈<) :: (Monad x, Contravariant c) => (a -> x b) -> c (x b) -> c (x a)
(<>≈<) = (=<<) >-> (>~>)
infixl 1 >≈<>
(>≈<>) :: (Monad x, Contravariant c) => c (x b) -> (a -> x b) -> c (x a)
(>≈<>) = flip (<>≈<)
-- special case where contravariant c is (flip (->) c')
infixl 1 <>≈
(<>≈) :: Monad x => (a -> x b) -> (x b -> c) -> x a -> c
(<>≈) = (=<<) >-> (>->)
infixl 1 ≈<>
(≈<>) :: Monad x => (x b -> c) -> (a -> x b) -> x a -> c
(≈<>) = flip (<>≈)

-- generalized >=> (4 types)
-- (a -> b -> x) -> (x -> b -> c) -> a -> b -> c
infixl 1 >>=>
(>>=>) :: (Monad b, Functor a) => a (b x) -> (x -> b c) -> a (b c)
(>>=>) = flip (<=<<)
infixl 1 <=<<
(<=<<) :: (Monad b, Functor a) => (x -> b c) -> a (b x) -> a (b c)
(<=<<) = (=<<) >-> (<-<)

-- ((x -> b) -> c) -> (x -> a) -> (a -> x -> b) -> c
infixl 1 <≈<<
(<≈<<) :: (Monad x, Contravariant c) => c (x b) -> x a -> c (a -> x b)
(<≈<<) = flip (>>≈>)
infixl 1 >>≈>
(>>≈>) :: (Monad x, Contravariant c) => x a -> c (x b) -> c (a -> x b)
(>>≈>) = (>>=) >-> (>~>)
-- special case where contravariant c is (flip (->) c')
infixl 1 <≈<
(<≈<) :: Monad x => (x b -> c) -> x a -> (a -> x b) -> c
(<≈<) = flip (>≈>)
infixl 1 >≈>
(>≈>) :: Monad x => x a -> (x b -> c) -> (a -> x b) -> c
(>≈>) = (>>=) >-> (>->)
