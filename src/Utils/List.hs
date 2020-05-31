module Utils.List where

rpad :: Int -> a -> [a] -> [a]
rpad n x xs = take n $ xs ++ repeat x
