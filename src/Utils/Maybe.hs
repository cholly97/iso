module Utils.Maybe where

import           Data.Maybe

nothingIf :: (a -> Bool) -> a -> Maybe a
nothingIf f a = if f a then Nothing else Just a

tryMaybe :: String -> (a -> b) -> Maybe a -> b
tryMaybe = maybe . error

tryFromMaybe :: String -> Maybe a -> a
tryFromMaybe = fromMaybe . error
