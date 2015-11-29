{-# LANGUAGE DeriveGeneric, DeriveTraversable, TemplateHaskell #-}
module Data.Either3
  (Either3 (..))
where

import Data.Tritraversable
import Data.Bifunctor.TH
import GHC.Generics


data Either3 α β γ = Left3 α | Mid3 β | Right3 γ
  deriving (Eq, Ord, Show, Read, Generic,
            Functor, Foldable, Traversable)

deriveBifunctor     ''Either3
deriveBifoldable    ''Either3
deriveBitraversable ''Either3

instance Trifunctor Either3 where trimap = trimapDefault

instance Trifoldable Either3 where trifoldMap = trifoldMapDefault

instance Tritraversable Either3 where
    tritraverse f _ _ (Left3  x) = Left3  <$> f x
    tritraverse _ g _ (Mid3   y) = Mid3   <$> g y
    tritraverse _ _ h (Right3 z) = Right3 <$> h z
