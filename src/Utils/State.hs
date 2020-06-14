module Utils.State where

import           Utils.Combinators

import           Control.Monad.Trans.State.Lazy

fState :: (s -> t) -> ((t -> t) -> (s -> s)) -> (s -> State t a) -> State s a
fState st ttss sSta = state $ \s ->
  let sta = sSta s
      t   = st s
  in  (evalState sta t, ttss -< execState sta -< s)
