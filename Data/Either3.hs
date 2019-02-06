{-# LANGUAGE DeriveGeneric, DeriveTraversable #-}

-- |
-- Module:      Data.Either3
-- Description: Three-way 'Either'
-- Copyright:   © 2019 Andy Morris
-- Licence:     AGPL-3.0-or-later
-- Maintainer:  hello@andy-morris.xyz
-- Stability:   experimental
-- Portability: DeriveGeneric, DeriveTraversable
module Data.Either3
  (Either3 (..), trifind3)
where

import Data.Tritraversable
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Monoid (First (..))
import GHC.Generics

-- | A three-alternative sum analogous to 'Either'.
data Either3 a b c = Left3 a | Mid3 b | Right3 c
  deriving (Eq, Ord, Show, Read, Generic,
            Functor, Foldable, Traversable)

instance Bifunctor (Either3 a) where bimap = bimapDefault

instance Bifoldable (Either3 a) where bifoldMap = bifoldMapDefault

instance Bitraversable (Either3 a) where bitraverse = tritraverse pure

instance Trifunctor Either3 where trimap = trimapDefault

instance Trifoldable Either3 where trifoldMap = trifoldMapDefault

instance Tritraversable Either3 where
    tritraverse f _ _ (Left3  x) = Left3  <$> f x
    tritraverse _ g _ (Mid3   y) = Mid3   <$> g y
    tritraverse _ _ h (Right3 z) = Right3 <$> h z


-- | Same as 'trifind3', except that the elements need not be the same
-- type.
trifind3 :: Trifoldable f
         => (a -> Bool) -> (b -> Bool) -> (c -> Bool)
         -> f a b c -> Maybe (Either3 a b c)
trifind3 pa pb pc =
    getFirst . trifoldMap (f Left3 pa) (f Mid3 pb) (f Right3 pc)
  where f k p x = First $ if p x then Just $ k x else Nothing
