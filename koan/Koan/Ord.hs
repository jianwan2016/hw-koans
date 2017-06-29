module Koan.Ord where

import Prelude hiding (max, maximum, min, minimum)

enrolled :: Bool
enrolled = False

-- | Get the greater of two values
max :: Ord a => a -> a -> a
-- max = error "TODO: implement max"
max x y = if x > y then x else y

-- | Get the lesser of two values
min :: Ord a => a -> a -> a
-- min = error "TODO: implement min"
min x y = if x < y then x else y

-- | Get the greatest element of the list.  The list must be finite and non-empty.
maximum :: Ord a => [a] -> a
-- maximum = error "TODO: implement maximum"
maximum (x:xs) = max x (maximum xs)

-- | Get the least element of the list.  The list must be finite and non-empty.
minimum :: Ord a => [a] -> a
-- minimum = error "TODO: implement minimum"
minimum (x:xs) = min x (minimum xs)

-- sort' :: Ord a => [a] -> [a]

-- | The 'sort' function implements a stable sorting algorithm.
sort :: Ord a => [a] -> [a]
-- sort = error "TODO: implement sort"
sort (x:xs) = sort (filter (< x) xs) ++ [x] ++ sort (filter (>= x) xs)
  where
    filter p ys = [xx | xx <- ys, p xx]

-- | The 'insert' function takes an element and a list and inserts the
-- element into the list at the first position where it is less
-- than or equal to the next element.  In particular, if the list
-- is sorted before the call, the result will also be sorted.
-- It is a special case of 'insertBy', which allows the programmer to
-- supply their own comparison function.
insert :: Ord a => a -> [a] -> [a]
-- insert = error "TODO: implement insert"
insert x xs = l ++ [x] ++ r
  where
    let split :: (a->Bool) -> [a] -> ([a], [a]) = 
    (l, r) = split (<=x) xs
