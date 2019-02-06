{-# LANGUAGE DeriveGeneric, StandaloneDeriving, UndecidableInstances #-}

-- |
-- Module:      Data.Trifunctor.Join
-- Description: Make a functor from all three arguments
-- Copyright:   © 2019 Andy Morris
-- Licence:     AGPL-3.0-or-later
-- Maintainer:  hello@andy-morris.xyz
-- Stability:   experimental
-- Portability: DeriveGeneric, StandaloneDeriving, UndecidableInstances
module Data.Trifunctor.Join
  (Join (..))
where

import Data.Tritraversable
import GHC.Generics (Generic)


-- | Make a 'Functor' over all arguments of a 'Trifunctor'.
newtype Join t a = Join {getJoin :: t a a a}
  deriving Generic

deriving instance Eq   (t a a a) => Eq   (Join t a)
deriving instance Ord  (t a a a) => Ord  (Join t a)
deriving instance Show (t a a a) => Show (Join t a)
deriving instance Read (t a a a) => Read (Join t a)


instance Trifunctor t => Functor (Join t) where
  fmap f = Join . trimap f f f . getJoin

instance Trifoldable t => Foldable (Join t) where
  foldMap f = trifoldMap f f f . getJoin

instance Tritraversable t => Traversable (Join t) where
  traverse f = fmap Join . tritraverse f f f . getJoin
