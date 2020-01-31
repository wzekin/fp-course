{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Applicative
import Course.Core
import Course.Functor
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.
newtype Compose f g a =
  Compose (f (g a))

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) => Functor (Compose f g) where
  f <$> Compose a = Compose ((f <$>) <$> a)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure a = Compose (pure (pure a))
  Compose f <*> Compose a = Compose (lift2 (<*>) f a)

-- Implement the pure function for an Applicative instance for Compose
-- Implement the (<*>) function for an Applicative instance for Compose
instance (Monad f, Monad g) => Monad (Compose f g)
-- Implement the (=<<) function for a Monad instance for Compose
                                                                 where
  (=<<) = error "impossible"
