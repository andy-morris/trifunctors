module Data.Trifoldable
  (Trifoldable (..),
   trifoldr', trifoldrM,
   trifoldl', trifoldlM,
   tritraverse_, trimapM_,
   trisequenceA_, trisequence_,
   triList, triany, triall, trisum, triproduct)
where

import Data.Monoid


-- | Foldable structures (like 'Foldable') but with three types of elements.
class Trifoldable f where
  {-# MINIMAL trifoldMap | trifoldr #-}
  -- | Combine all elements together.
  trifold :: Monoid m => f m m m -> m
  trifold = trifoldMap id id id
  -- | Map all elements to a common monoidal type and combine the results.
  trifoldMap :: Monoid m => (a -> m) -> (b -> m) -> (c -> m) -> f a b c -> m
  trifoldMap f g h =
      trifoldr (mappend . f) (mappend . g) (mappend . h) mempty
  -- | Combine elements in a right-associative way, like 'foldr'.
  trifoldr :: (a -> d -> d) -> (b -> d -> d) -> (c -> d -> d)
           -> d -> f a b c -> d
  trifoldr f g h z t =
      appEndo (trifoldMap (Endo . f) (Endo . g) (Endo . h) t) z
  -- | Combine elements in a left-associative way, like 'foldl'.
  trifoldl :: (d -> a -> d) -> (d -> b -> d) -> (d -> c -> d)
           -> d -> f a b c -> d
  trifoldl f g h z t =
      appEndo (getDual (trifoldMap (dual f) (dual g) (dual h) t)) z
    where dual k = Dual . Endo . flip k


instance Trifoldable (,,) where
  trifoldMap f g h (x, y, z) = f x <> g y <> h z


-- | Strict version of 'trifoldr'.
trifoldr' :: Trifoldable f
          => (a -> d -> d) -> (b -> d -> d) -> (c -> d -> d)
          -> d -> f a b c -> d
trifoldr' f g h z₀ t = trifoldl (s f) (s g) (s h) id t z₀
  where s k ℓ x z = ℓ $! k x z

-- | Strict version of 'trifoldl'.
trifoldl' :: Trifoldable f
          => (d -> a -> d) -> (d -> b -> d) -> (d -> c -> d)
          -> d -> f a b c -> d
trifoldl' f g h z₀ t = trifoldr (s f) (s g) (s h) id t z₀
  where s k x ℓ z = ℓ $! k z x


-- | Monadic version of 'trifoldr'.
trifoldrM :: (Trifoldable f, Monad m)
          => (a -> d -> m d) -> (b -> d -> m d) -> (c -> d -> m d) -> d
          -> f a b c -> m d
trifoldrM f g h z₀ t = trifoldl (m f) (m g) (m h) return t z₀
  where m k ℓ x z = k x z >>= ℓ

-- | Monadic version of 'trifoldl'.
trifoldlM :: (Trifoldable f, Monad m)
          => (d -> a -> m d) -> (d -> b -> m d) -> (d -> c -> m d) -> d
          -> f a b c -> m d
trifoldlM f g h z₀ t = trifoldr (m f) (m g) (m h) return t z₀
  where m k x ℓ z = k z x >>= ℓ


-- | Perform some effectful actions to all elements left-to-right, and ignore
-- the results.
tritraverse_ :: (Trifoldable f, Applicative g)
             => (a -> g z) -> (b -> g y) -> (c -> g x) -> f a b c -> g ()
tritraverse_ f g h = trifoldr (m f) (m g) (m h) (pure ())
  where m k x y = k x *> y

-- | Same as 'tritraverse_'.
trimapM_ :: (Trifoldable f, Monad m)
         => (a -> m z) -> (b -> m y) -> (c -> m x) -> f a b c -> m ()
trimapM_ = tritraverse_

-- | Perform all actions contained in the structure, but ignore the values.
trisequenceA_ :: (Trifoldable f, Applicative g) => f (g a) (g b) (g c) -> g ()
trisequenceA_ = tritraverse_ id id id

-- | Same as 'trisequenceA_'.
trisequence_ :: (Trifoldable f, Monad m) => f (m a) (m b) (m c) -> m ()
trisequence_ = trisequenceA_


-- | Put all elements into a list.
triList :: Trifoldable f => f a a a -> [a]
triList t = appEndo (trifoldMap cons cons cons t) []
  where cons x = Endo (x :)


-- | Whether any of the elements satisfy the given predicates.
triany :: Trifoldable f
       => (a -> Bool) -> (b -> Bool) -> (c -> Bool) -> f a b c -> Bool
triany f g h = getAny . trifoldMap (Any . f) (Any . g) (Any . h)

-- | Whether all of the elements satisfy the given predicates.
triall :: Trifoldable f
       => (a -> Bool) -> (b -> Bool) -> (c -> Bool) -> f a b c -> Bool
triall f g h = getAll . trifoldMap (All . f) (All . g) (All . h)


-- | The sum of all contained elements.
trisum :: (Trifoldable f, Num a) => f a a a -> a
trisum = getSum . trifoldMap Sum Sum Sum

-- | The product of all contained elements.
triproduct :: (Trifoldable f, Num a) => f a a a -> a
triproduct = getProduct . trifoldMap Product Product Product
