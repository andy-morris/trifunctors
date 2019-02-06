{-# LANGUAGE DeriveGeneric, DeriveTraversable, PolyKinds #-}

-- |
-- Module:      Data.Trifunctor.Tritannen
-- Description: Compose a functor with a trifunctor
-- Copyright:   © 2019 Andy Morris
-- Licence:     AGPL-3.0-or-later
-- Maintainer:  hello@andy-morris.xyz
-- Stability:   experimental
-- Portability: DeriveGeneric, DeriveTraversable, PolyKinds
module Data.Trifunctor.Tritannen (Tritannen (..)) where
  -- if anyone can think of better names i'd love to hear them


import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable

import Data.Tritraversable

import GHC.Generics (Generic)


-- | Compose a 'Functor' on the outside of a 'Trifunctor', the opposite
-- arrangement to 'Data.Trifunctor.Trifff'.
--
-- (The names work better for \'Biff\' and \'Tannen\'.)
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
