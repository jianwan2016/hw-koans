module Koan.Function where

import Prelude hiding (($), (.))

import Koan.Applicative as K
import Koan.Functor     as K
import Koan.Monad       as K

enrolled :: Bool
enrolled = True

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f1 f2 a = f1 (f2 a)

flip :: (a -> b -> c) -> b -> a -> c
flip f b a = f a b

-- Hint: You're already implemented this.
mapFunction :: (a -> b) -> (r -> a) -> r -> b
mapFunction = (.)
-- mapFunction = error "TODO: Implement mapFunction"

applyFunction :: (r -> a -> b) -> (r -> a) -> r -> b
applyFunction f f2 r = f r (f2 r)

bindFunction :: (a -> r -> b) -> (r -> a) -> r -> b
bindFunction f f2 x = f (f2 x) x

instance K.Functor ((->) r) where
  fmap = mapFunction

instance K.Applicative ((->) r) where
  -- pure b = \_ -> b
  pure b _ = b
  (<*>) = applyFunction

instance K.Monad ((->) r) where
  -- (>>=) = error "TODO: Implement Monad (>>=) for (->)"
  (>>=) = Prelude.flip bindFunction
