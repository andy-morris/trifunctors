{-# LANGUAGE DeriveGeneric, DeriveTraversable, TemplateHaskell #-}
module Data.Either3
  (Either3 (..))
where

import Data.Tritraversable
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import GHC.Generics


-- | Same as 'Either', but with three alternatives.
data Either3 α β γ = Left3 α | Mid3 β | Right3 γ
  deriving (Eq, Ord, Show, Read, Generic,
            Functor, Foldable, Traversable)

instance Bifunctor (Either3 α) where bimap = bimapDefault

instance Bifoldable (Either3 α) where bifoldMap = bifoldMapDefault

instance Bitraversable (Either3 α) where bitraverse = tritraverse pure

instance Trifunctor Either3 where trimap = trimapDefault

instance Trifoldable Either3 where trifoldMap = trifoldMapDefault

instance Tritraversable Either3 where
    tritraverse f _ _ (Left3  x) = Left3  <$> f x
    tritraverse _ g _ (Mid3   y) = Mid3   <$> g y
    tritraverse _ _ h (Right3 z) = Right3 <$> h z
