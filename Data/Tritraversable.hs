-- |
-- Module:      Data.Tritraversable
-- Description: Three-argument 'Traversable'
-- Copyright:   © 2019 Andy Morris
-- Licence:     AGPL-3.0-or-later
-- Maintainer:  hello@andy-morris.xyz
-- Stability:   experimental
-- Portability: portable
module Data.Tritraversable
  (module Data.Trifunctor,
   module Data.Trifoldable,
   Tritraversable (..),
   trimapDefault, trifoldMapDefault,
   trifor, triforM)
where

import Data.Trifunctor
import Data.Trifoldable
import Data.Functor.Identity
import Control.Applicative


-- | Structures which can be traversed in order while keeping the shape, like
-- 'Traversable'.
--
-- The laws are analogous to those for 'Traversable':
--
-- For 'tritraverse':
--
-- @
-- -- "naturality"
-- 'tritraverse' (t . f) (t . g) (t . h) === t . 'tritraverse' f g h
--
-- -- "identity"
-- 'tritraverse' 'Identity' 'Identity' 'Identity' === 'Identity'
--
-- -- "composition"
-- 'Data.Functor.Compose.Compose' . 'fmap' ('tritraverse' g1 g2 g3) . 'tritraverse' f1 f2 f3 ===
--   'tritraverse' ('Data.Functor.Compose.Compose' . 'fmap' g1 . f1)
--               ('Data.Functor.Compose.Compose' . 'fmap' g2 . f2)
--               ('Data.Functor.Compose.Compose' . 'fmap' g3 . f3)
-- @
--
-- For 'trisequenceA':
--
-- @
-- -- "naturality"
-- t . 'trisequenceA' === 'trisequenceA' . 'fmap' t
--
-- -- "identity"
-- 'trisequenceA' . 'fmap' 'Identity' === 'Identity'
--
-- -- "composition"
-- 'trisequenceA' . 'fmap' 'Data.Functor.Compose.Compose' ===
--    'Data.Functor.Compose.Compose' . 'fmap' 'trisequenceA' . 'trisequenceA'
-- @
class (Trifunctor f, Trifoldable f) => Tritraversable f where
  {-# MINIMAL tritraverse | trisequenceA #-}
  -- | Runs the effectful actions at each element and collect the results in
  -- a new structure of the same shape as the old.
  tritraverse :: Applicative g
              => (a -> g x) -> (b -> g y) -> (c -> g z)
              -> f a b c -> g (f x y z)
  tritraverse f g h = trisequenceA . trimap f g h

  -- | Runs all the contained actions and collects the results in a structure
  -- of the same shape.
  trisequenceA :: Applicative g => f (g a) (g b) (g c) -> g (f a b c)
  trisequenceA = tritraverse id id id

  -- | Same as 'tritraverse'.
  trimapM :: Monad m
          => (a -> m x) -> (b -> m y) -> (c -> m z)
          -> f a b c -> m (f x y z)
  trimapM = tritraverse

  -- | Same as 'trisequencex.
  trisequence :: Monad m => f (m a) (m b) (m c) -> m (f a b c)
  trisequence = trisequenceA



instance Tritraversable (,,) where
  tritraverse f g h (x, y, z) = (,,) <$> f x <*> g y <*> h z


-- | Suitable default definition for 'trimap'.
trimapDefault :: Tritraversable f
              => (a -> x) -> (b -> y) -> (c -> z) -> f a b c -> f x y z
trimapDefault f g h = runIdentity . tritraverse (id' f) (id' g) (id' h)
  where id' k = Identity . k


-- | Suitable default definition for 'trifoldMap'.
trifoldMapDefault :: (Tritraversable f, Monoid m)
                  => (a -> m) -> (b -> m) -> (c -> m) -> f a b c -> m
trifoldMapDefault f g h = getConst . tritraverse (c f) (c g) (c h)
  where c k = Const . k


-- | Same as 'tritraverse' but with the argument order swapped.
trifor :: (Tritraversable f, Applicative g)
       => f a b c -> (a -> g x) -> (b -> g y) -> (c -> g z) -> g (f x y z)
trifor t f g h = tritraverse f g h t

-- | Same as 'trimapM' but with the argument order swapped.
triforM :: (Tritraversable f, Monad m)
        => f a b c -> (a -> m x) -> (b -> m y) -> (c -> m z) -> m (f x y z)
triforM t f g h = trimapM f g h t
