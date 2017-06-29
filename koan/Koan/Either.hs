module Koan.Either where

import Control.Arrow

import Data.Bifunctor
import Prelude        hiding (Either (..), either, isLeft, isRight, lefts, rights)

enrolled :: Bool
enrolled = True

data Either a b = Left a | Right b

isLeft :: Either a b -> Bool
isLeft (Left _)  = True
isLeft (Right _) = False

isRight :: Either a b -> Bool
isRight (Left _)  = False
isRight (Right _) = True

lefts :: [Either a b] -> [a]
lefts (Left a:xs)   = a : lefts xs
lefts (Right _: xs) = lefts xs
lefts _             = []

rights :: [Either a b] -> [b]
rights (x:xs) = case x of
  Left _  -> rights xs
  Right a -> a: rights xs
rights _ = []
-- rights (Right a: xs) = a : rights xs
-- rights (Left _ : xs) = rights xs
-- rights _             = []

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left a)  = f a
either _ f (Right b) = f b

-- partition' :: Either a b -> [a]
-- If you want a challenge, try to implement this using `foldr`
partition :: [Either a b] -> ([a], [b])
-- solution 1
--partition xs = (lefts xs, rights xs)
-- partition _  = []

-- solution 2
-- partition = lefts &&& rights

-- solution 3
partition = foldr go ([], [])
  -- where go :: Either a b -> ([a], [b]) -> ([a], [b])
  where go :: Either a b -> ([a], [b]) -> ([a], [b])
        go (Left a) (as, bs)  = (a: as, bs)
        go (Right b) (as, bs) = (as, b: bs)

mapEither :: (b -> c) -> Either a b -> Either a c
mapEither f (Right b) = Right (f b)
mapEither _ (Left a)  =  Left a

bimapEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
bimapEither f _ (Left a)  = Left (f a)
bimapEither _ f (Right b) = Right (f b)

applyEither :: Either a (b -> c) -> Either a b -> Either a c
applyEither (Left a) _          = Left a
-- applyEither (Left a1) (Left a2) = Left a1
applyEither (Right f) (Right b) = Right (f b)
applyEither (Right f) (Left a)  = Left a

bindEither :: (b -> Either a c) -> Either a b -> Either a c
bindEither f (Left a)  = Left a
bindEither f (Right b) = f b

instance Functor (Either a) where
  fmap = mapEither

instance Bifunctor Either where
  bimap = bimapEither

instance Applicative (Either a) where
  pure = Right
  (<*>) = applyEither

instance Monad (Either a) where
 (>>=) = flip bindEither
