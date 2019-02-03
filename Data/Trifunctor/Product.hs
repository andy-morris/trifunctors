{-# LANGUAGE DeriveGeneric, DeriveTraversable, PolyKinds #-}

module Data.Trifunctor.Product
  (Product (..))
where


import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable

import Data.Tritraversable

import GHC.Generics (Generic)


data Product s t a b c = Pair (s a b c) (t a b c)
  deriving (Eq, Ord, Show, Read, Generic,
            Functor, Foldable, Traversable)


instance (Trifunctor s, Trifunctor t) => Bifunctor (Product s t a) where
  bimap g h (Pair x y) = Pair (trimap id g h x) (trimap id g h y)

instance (Trifoldable s, Trifoldable t) => Bifoldable (Product s t a) where
  bifoldMap g h (Pair x y) =
    trifoldMap (const mempty) g h x <> trifoldMap (const mempty) g h y

instance (Tritraversable s, Tritraversable t)
    => Bitraversable (Product s t a) where
  bitraverse g h (Pair x y) =
    Pair <$> tritraverse pure g h x <*> tritraverse pure g h y


instance (Trifunctor s, Trifunctor t) => Trifunctor (Product s t) where
  trimap f g h (Pair x y) = Pair (trimap f g h x) (trimap f g h y)

instance (Trifoldable s, Trifoldable t)
    => Trifoldable (Product s t) where
  trifoldMap f g h (Pair x y) = trifoldMap f g h x <> trifoldMap f g h y

instance (Tritraversable s, Tritraversable t)
    => Tritraversable (Product s t) where
  tritraverse f g h (Pair x y) =
    Pair <$> tritraverse f g h x <*> tritraverse f g h y

