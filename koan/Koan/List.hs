module Koan.List where

import           Koan.Applicative as K
import           Koan.Functor     as K
import           Koan.Monad       as K
import           Prelude          hiding (concat, head, init, last, reverse, tail, (++))


enrolled :: Bool
enrolled = True

-- Example:
--   head [1, 2, 3, 4] = 1
head :: [a] -> a
head (x:xs) = x
head []     = error "no elements"

-- Example:
--   tail [1, 2, 3, 4] = [2, 3, 4]
tail :: [a] -> [a]
tail (x:xs) = xs

-- Example:
--   last [1, 2, 3, 4] = 4
last :: [a] -> a
last (x:xs) = case xs of
  [] -> x
  _  -> last xs
last _ = error "No elements"

reverse' :: [a] -> [a] -> [a]
reverse' acc (x:xs) = reverse' (x:acc) xs
reverse' acc _      = acc

-- Example:
--   reverse [1, 2, 3, 4] = [4, 3, 2, 1]
reverse :: [a] -> [a]
reverse = reverse' []
-- reverse = error "TODO: implement reverse"

-- Example:
--   [1, 2] ++ [3, 4] = [1, 2, 3, 4]
(++) :: [a] -> [a] -> [a]
(++) (x:xs) y = x : xs ++ y
(++) x []     = x
(++) [] y     = y

-- Example:
--   concat [[1, 2], [3, 4]] = [1, 2, 3, 4]
concat :: [[a]] -> [a]
concat (x:xs) = x ++ concat xs
concat _      = []

-- tails' :: [[a]] -> [a] -> [[a]]
-- tails' acc x= tails' (x:acc) (tail x)

-- tails :: [a] -> [[a]]
-- tails  = tails' [[]]

-- Example:
--   tails [1, 2, 3] = [[1, 2, 3], [2, 3], [3], []]
tails :: [a] -> [[a]]
tails (x:xs) = (x:xs) : tails xs
tails _      = [[]]

-- Example:
--   mapList show [1, 2, 3, 4] = ["1", "2", "3", "4"]
mapList :: (a -> b) -> [a] -> [b]
mapList f (x:xs) = f x : mapList f xs

-- Example:
--   filterList even [1, 2, 3, 4] = [2, 4]
filterList :: (a -> Bool) -> [a] -> [a]
filterList f (x:xs) = if f x then x : filterList f xs
  else filterList f xs
filterList _ _ = []

-- Example:
--   foldlList (+) 0 [1, 2, 3] = 6
foldlList :: (b -> a -> b) -> b -> [a] -> b
foldlList f acc (x:xs) = foldlList f (f acc x) xs
foldlList _ acc _      = acc

foldrList :: (a -> b -> b) -> b -> [a] -> b
foldrList f acc (x:xs) = foldrList f (f x acc) xs
foldrList _ acc _      = acc

-- Note that those are square brackets, not round brackets.
applyList :: [a -> b] -> [a] -> [b]
applyList (f:fs) (x:xs) = f x : applyList fs xs

bindList :: (a -> [b]) -> [a] -> [b]
bindList f (x:xs) = f x

instance K.Functor [] where
  fmap = error "TODO: Implement fmap for ([a])"

instance K.Applicative [] where
  pure = error "TODO: Implement Applicative pure for []"
  (<*>) = error "TODO: Implement Applicative (<*>) for []"

instance K.Monad [] where
  (>>=) = error "TODO: Implement Monad (>>=) for []"
