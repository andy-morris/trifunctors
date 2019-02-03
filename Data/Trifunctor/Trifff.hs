{-# LANGUAGE DeriveGeneric, DeriveTraversable #-}

module Data.Trifunctor.Trifff (Trifff (..)) where
  -- if anyone can think of an analogue to Biff & Tannen for tri i'd love
  -- to hear it


import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable

import Data.Tritraversable

import GHC.Generics (Generic)


newtype Trifff t f g h a b c =
    Trifff {runTrifff :: t (f a) (g b) (h c)}
  deriving (Eq, Ord, Read, Show, Generic)

instance (Trifunctor t, Functor f, Functor g, Functor h) =>
    Trifunctor (Trifff t f g h) where
  trimap f g h (Trifff x) =
    Trifff $ trimap (fmap f) (fmap g) (fmap h) x

instance (Trifoldable t, Foldable f, Foldable g, Foldable h) =>
    Trifoldable (Trifff t f g h) where
  trifoldMap f g h (Trifff x) =
    trifoldMap (foldMap f) (foldMap g) (foldMap h) x

instance (Tritraversable t, Traversable f, Traversable g, Traversable h) =>
    Tritraversable (Trifff t f g h) where
  tritraverse f g h (Trifff x) =
    Trifff <$> tritraverse (traverse f) (traverse g) (traverse h) x


instance (Trifunctor t, Functor g, Functor h) =>
    Bifunctor (Trifff t f g h a) where
  bimap g h (Trifff x) =
    Trifff $ trimap id (fmap g) (fmap h) x

instance (Trifoldable t, Foldable g, Foldable h) =>
    Bifoldable (Trifff t f g h a) where
  bifoldMap g h (Trifff x) =
    trifoldMap (const mempty) (foldMap g) (foldMap h) x

instance (Tritraversable t, Traversable g, Traversable h) =>
    Bitraversable (Trifff t f g h a) where
  bitraverse g h (Trifff x) =
    Trifff <$> tritraverse pure (traverse g) (traverse h) x


instance (Trifunctor t, Functor h) => Functor (Trifff t f g h a b) where
  fmap h (Trifff x) = Trifff $ trimap id id (fmap h) x

instance (Trifoldable t, Foldable h) => Foldable (Trifff t f g h a b) where
  foldMap h (Trifff x) =
    trifoldMap (const mempty) (const mempty) (foldMap h) x

instance (Tritraversable t, Traversable h) =>
    Traversable (Trifff t f g h a b) where
  traverse h (Trifff x) = Trifff <$> tritraverse pure pure (traverse h) x

