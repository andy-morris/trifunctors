{-# LANGUAGE DeriveGeneric, DeriveTraversable, LambdaCase, PolyKinds #-}

module Data.Trifunctor.Sum
  (Sum (..))
where


import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable

import Data.Tritraversable

import GHC.Generics (Generic)


data Sum s t a b c = Inl (s a b c) | Inr (t a b c)
  deriving (Eq, Ord, Show, Read, Generic,
            Functor, Foldable, Traversable)


instance (Trifunctor s, Trifunctor t) => Bifunctor (Sum s t a) where
  bimap g h = \case
    Inl x -> Inl $ trimap id g h x
    Inr y -> Inr $ trimap id g h y

instance (Trifoldable s, Trifoldable t) => Bifoldable (Sum s t a) where
  bifoldMap g h = \case
    Inl x -> trifoldMap (const mempty) g h x
    Inr y -> trifoldMap (const mempty) g h y

instance (Tritraversable s, Tritraversable t) => Bitraversable (Sum s t a) where
  bitraverse g h = \case
    Inl x -> Inl <$> tritraverse pure g h x
    Inr y -> Inr <$> tritraverse pure g h y


instance (Trifunctor s, Trifunctor t) => Trifunctor (Sum s t) where
  trimap f g h = \case
    Inl x -> Inl $ trimap f g h x
    Inr y -> Inr $ trimap f g h y

instance (Trifoldable s, Trifoldable t) => Trifoldable (Sum s t) where
  trifoldMap f g h = \case
    Inl x -> trifoldMap f g h x
    Inr y -> trifoldMap f g h y

instance (Tritraversable s, Tritraversable t) => Tritraversable (Sum s t) where
  tritraverse f g h = \case
    Inl x -> Inl <$> tritraverse f g h x
    Inr y -> Inr <$> tritraverse f g h y
