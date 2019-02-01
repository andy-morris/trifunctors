{-# LANGUAGE DeriveGeneric, DeriveTraversable #-}

module Data.Trifunctor.Tritannen (Tritannen (..)) where
  -- if anyone can think of an analogue to Biff & Tannen for tri i'd love
  -- to hear it


import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable

import Data.Tritraversable

import GHC.Generics (Generic)


newtype Tritannen f t α β γ =
    Tritannen {runTritannen :: f (t α β γ)}
  deriving (Eq, Ord, Read, Show, Generic,
            Functor, Foldable, Traversable)

instance (Functor f, Trifunctor t) => Trifunctor (Tritannen f t) where
  trimap f g h = Tritannen . fmap (trimap f g h) . runTritannen

instance (Foldable f, Trifoldable t) => Trifoldable (Tritannen f t) where
  trifoldMap f g h = foldMap (trifoldMap f g h) . runTritannen

instance (Traversable f, Tritraversable t) =>
    Tritraversable (Tritannen f t) where
  tritraverse f g h =
    fmap Tritannen . traverse (tritraverse f g h) . runTritannen


instance (Functor f, Bifunctor (t α)) => Bifunctor (Tritannen f t α) where
  bimap g h = Tritannen . fmap (bimap g h) . runTritannen

instance (Foldable f, Bifoldable (t α)) =>
    Bifoldable (Tritannen f t α) where
  bifoldMap g h = foldMap (bifoldMap g h) . runTritannen

instance (Traversable f, Bitraversable (t α)) =>
    Bitraversable (Tritannen f t α) where
  bitraverse g h =
    fmap Tritannen . traverse (bitraverse g h) . runTritannen
