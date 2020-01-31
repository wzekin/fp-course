{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Traversable where

import Course.Applicative
import Course.Compose
import Course.Core
import Course.ExactlyOne
import Course.Functor
import Course.List
import Course.Optional

-- | All instances of the `Traversable` type-class must satisfy two laws. These
-- laws are not checked by the compiler. These laws are given as:
--
-- * The law of naturality
--   `∀f g. f . traverse g ≅ traverse (f . g)`
--
-- * The law of identity
--   `∀x. traverse ExactlyOne x ≅ ExactlyOne x`
--
-- * The law of composition
--   `∀f g. traverse ((g <$>) . f) ≅ (traverse g <$>) . traverse f`
class Functor t =>
      Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

instance Traversable List where
  traverse :: Applicative f => (a -> f b) -> List a -> f (List b)
  traverse f = foldRight (\a b -> (:.) <$> f a <*> b) (pure Nil)

instance Traversable ExactlyOne where
  traverse :: Applicative f => (a -> f b) -> ExactlyOne a -> f (ExactlyOne b)
  traverse f a = ExactlyOne <$> f (runExactlyOne a)

instance Traversable Optional where
  traverse :: Applicative f => (a -> f b) -> Optional a -> f (Optional b)
  traverse f (Full a) = Full <$> f a
  traverse _ Empty = pure Empty

-- | Sequences a traversable value of structures to a structure of a traversable value.
--
-- >>> sequenceA (ExactlyOne 7 :. ExactlyOne 8 :. ExactlyOne 9 :. Nil)
-- ExactlyOne [7,8,9]
--
-- >>> sequenceA (Full (ExactlyOne 7))
-- ExactlyOne (Full 7)
--
-- >>> sequenceA (Full (*10)) 6
-- Full 60
sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
sequenceA = traverse id

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose a) = Compose <$> traverse (traverse f) a

-- Implement the traverse function for a Traversable instance for Compose
-- | The `Product` data type contains one value from each of the two type constructors.
data Product f g a =
  Product (f a)
          (g a)

instance (Functor f, Functor g) => Functor (Product f g) where
  f <$> Product a b = Product (f <$> a) (f <$> b)

-- Implement the (<$>) function for a Functor instance for Product
instance (Traversable f, Traversable g) => Traversable (Product f g) where
  traverse f (Product a b) = Product <$> traverse f a <*> traverse f b

-- Implement the traverse function for a Traversable instance for Product
-- | The `Coproduct` data type contains one value from either of the two type constructors.
data Coproduct f g a
  = InL (f a)
  | InR (g a)

instance (Functor f, Functor g) => Functor (Coproduct f g) where
  f <$> InL a = InL (f <$> a)
  f <$> InR a = InR (f <$> a)

-- Implement the (<$>) function for a Functor instance for Coproduct
instance (Traversable f, Traversable g) => Traversable (Coproduct f g)
-- Implement the traverse function for a Traversable instance for Coproduct
                                                                            where
  traverse f (InL a) = InL <$> traverse f a
  traverse f (InR a) = InR <$> traverse f a
