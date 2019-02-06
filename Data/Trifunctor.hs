-- |
-- Module:      Data.Trifunctor
-- Description: Three-argument functors
-- Copyright:   © 2019 Andy Morris
-- Licence:     AGPL-3.0-or-later
-- Maintainer:  hello@andy-morris.xyz
-- Stability:   experimental
-- Portability: portable
module Data.Trifunctor (Trifunctor (..)) where

-- |
-- A trifunctor is like a (covariant) 'Functor' or 'Data.Bifunctor.Bifunctor',
-- but with three arguments.
-- If you define 'trimap':
--
-- @
-- trimap id id id === id
-- @
--
-- If you define 'first3', 'second3', and 'third3':
--
-- @
-- first3 id === id
-- second3 id === id
-- third3 id === id
-- @
--
-- If both:
--
-- @
-- trimap f g h === first3 f . second3 g . third3 h
-- @
class Trifunctor f where
    {-# MINIMAL trimap | (first3, second3, third3) #-}
    -- | Map over all three arguments at once.
    trimap :: (a -> a') -> (b -> b') -> (c -> c') -> f a b c -> f a' b' c'
    trimap f g h = first3 f . second3 g . third3 h
    -- | Map covariantly over the first argument.
    first3 :: (a -> a') -> f a b c -> f a' b c
    first3  f = trimap f  id id
    -- | Map covariantly over the second argument.
    second3 :: (b -> b') -> f a b c -> f a  b' c
    second3 g = trimap id g  id
    -- | Map covariantly over the third argument.
    third3 :: (c -> c') -> f a b c -> f a b c'
    third3  h = trimap id id h

instance Trifunctor (,,) where trimap f g h (x, y, z) = (f x, g y, h z)
