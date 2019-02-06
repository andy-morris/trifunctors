{-# LANGUAGE DeriveGeneric, PolyKinds #-}

-- |
-- Module:      Data.Trifunctor.Rotate
-- Description: Rotate arguments left or right
-- Copyright:   © 2019 Andy Morris
-- Licence:     AGPL-3.0-or-later
-- Maintainer:  hello@andy-morris.xyz
-- Stability:   experimental
-- Portability: DeriveGeneric, PolyKinds
module Data.Trifunctor.Rotate
  (RotL (..), RotR (..))
where

import Data.Bifunctor
import Data.Tritraversable
import GHC.Generics


-- | Rotate the last three arguments of a type to the left.
newtype RotL f b c a = RotL {unRotL :: f a b c}
  deriving (Eq, Ord, Show, Read, Generic)

instance Trifunctor f => Functor (RotL f b c) where
    fmap f = RotL . first3 f . unRotL

instance Trifunctor f => Bifunctor (RotL f b) where
    bimap h f = RotL . trimap f id h . unRotL

instance Trifunctor f => Trifunctor (RotL f) where
    trimap g h f = RotL . trimap f g h . unRotL

instance Trifoldable f => Trifoldable (RotL f) where
    trifoldMap g h f = trifoldMap f g h . unRotL

instance Tritraversable f => Tritraversable (RotL f) where
    tritraverse g h f = fmap RotL . tritraverse f g h . unRotL


-- | Rotate the last three arguments of a type to the right.
newtype RotR f c a b = RotR {unRotR :: f a b c}
  deriving (Eq, Ord, Show, Read, Generic)

instance Trifunctor f => Functor (RotR f c a) where
    fmap g = RotR . second3 g . unRotR

instance Trifunctor f => Bifunctor (RotR f c) where
    bimap f g = RotR . trimap f g id . unRotR

instance Trifunctor f => Trifunctor (RotR f) where
    trimap h f g = RotR . trimap f g h . unRotR

instance Trifoldable f => Trifoldable (RotR f) where
    trifoldMap h f g = trifoldMap f g h . unRotR

instance Tritraversable f => Tritraversable (RotR f) where
    tritraverse h f g = fmap RotR . tritraverse f g h . unRotR
