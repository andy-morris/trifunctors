{-# LANGUAGE DeriveGeneric, DeriveTraversable #-}

module Data.Trifunctor.Tritannen (Tritannen (..)) where
  -- if anyone can think of an analogue to Biff & Tannen for tri i'd love
  -- to hear it


import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable

import Data.Tritraversable

import GHC.Generics (Generic)


newtype Tritannen f t a b c = Tritannen {runTritannen :: f (t a b c)}
  deriving (Eq, Ord, Read, Show, Generic)


instance (Functor f, Trifunctor t) => Trifunctor (Tritannen f t) where
  trimap f g h = Tritannen . fmap (trimap f g h) . runTritannen

instance (Foldable f, Trifoldable t) => Trifoldable (Tritannen f t) where
  trifoldMap f g h = foldMap (trifoldMap f g h) . runTritannen

instance (Traversable f, Tritraversable t) =>
    Tritraversable (Tritannen f t) where
  tritraverse f g h =
    fmap Tritannen . traverse (tritraverse f g h) . runTritannen


instance (Functor f, Trifunctor t) => Bifunctor (Tritannen f t a) where
  bimap g h = Tritannen . fmap (trimap id g h) . runTritannen

instance (Foldable f, Trifoldable t) => Bifoldable (Tritannen f t a) where
  bifoldMap g h = foldMap (trifoldMap (const mempty) g h) . runTritannen

instance (Traversable f, Tritraversable t) =>
    Bitraversable (Tritannen f t a) where
  bitraverse g h =
    fmap Tritannen . traverse (tritraverse pure g h) . runTritannen


instance (Functor f, Trifunctor t) => Functor (Tritannen f t a b) where
  fmap h = Tritannen . fmap (trimap id id h) . runTritannen

instance (Foldable f, Trifoldable t) => Foldable (Tritannen f t a b) where
  foldMap h =
    foldMap (trifoldMap (const mempty) (const mempty) h) . runTritannen

instance (Traversable f, Tritraversable t) =>
    Traversable (Tritannen f t a b) where
  traverse h =
    fmap Tritannen . traverse (tritraverse pure pure h) . runTritannen
