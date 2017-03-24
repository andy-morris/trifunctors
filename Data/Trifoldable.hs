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
    trifold    ∷ Monoid m ⇒ f m m m → m
    -- | Map all elements to a common monoidal type and combine the results.
    trifoldMap ∷ Monoid m ⇒ (α → m) → (β → m) → (γ → m) → f α β γ → m
    -- | Combine elements in a right-associative way, like 'foldr'.
    trifoldr   ∷ (α → δ → δ) → (β → δ → δ) → (γ → δ → δ) → δ → f α β γ → δ
    -- | Combine elements in a left-associative way, like 'foldl'.
    trifoldl   ∷ (δ → α → δ) → (δ → β → δ) → (δ → γ → δ) → δ → f α β γ → δ

    -- stolen from bifoldable, obv
    trifold = trifoldMap id id id
    trifoldMap f g h =
        trifoldr (mappend . f) (mappend . g) (mappend . h) mempty
    trifoldr f g h z t =
        appEndo (trifoldMap (Endo . f) (Endo . g) (Endo . h) t) z
    trifoldl f g h z t =
        appEndo (getDual (trifoldMap (dual f) (dual g) (dual h) t)) z
      where dual k = Dual . Endo . flip k


instance Trifoldable (,,) where
    trifoldMap f g h (x, y, z) = f x <> g y <> h z


-- | Strict version of 'trifoldr'.
trifoldr' ∷ Trifoldable f
          ⇒ (α → δ → δ) → (β → δ → δ) → (γ → δ → δ) → δ → f α β γ → δ
trifoldr' f g h z₀ t = trifoldl (s f) (s g) (s h) id t z₀
  where s k ℓ x z = ℓ $! k x z

-- | Strict version of 'trifoldl'.
trifoldl' ∷ Trifoldable f
          ⇒ (δ → α → δ) → (δ → β → δ) → (δ → γ → δ) → δ → f α β γ → δ
trifoldl' f g h z₀ t = trifoldr (s f) (s g) (s h) id t z₀
  where s k x ℓ z = ℓ $! k z x


-- | Monadic version of 'trifoldr'.
trifoldrM ∷ (Trifoldable f, Monad m)
          ⇒ (α → δ → m δ) → (β → δ → m δ) → (γ → δ → m δ) → δ
          → f α β γ → m δ
trifoldrM f g h z₀ t = trifoldl (m f) (m g) (m h) return t z₀
  where m k ℓ x z = k x z >>= ℓ

-- | Monadic version of 'trifoldl'.
trifoldlM ∷ (Trifoldable f, Monad m)
          ⇒ (δ → α → m δ) → (δ → β → m δ) → (δ → γ → m δ) → δ
          → f α β γ → m δ
trifoldlM f g h z₀ t = trifoldr (m f) (m g) (m h) return t z₀
  where m k x ℓ z = k z x >>= ℓ


-- | Perform some effectful actions to all elements left-to-right, and ignore
-- the results.
tritraverse_ ∷ (Trifoldable f, Applicative g)
             ⇒ (α → g χ) → (β → g ψ) → (γ → g ω) → f α β γ → g ()
tritraverse_ f g h = trifoldr (m f) (m g) (m h) (pure ())
  where m k x y = k x *> y

-- | Same as 'tritraverse_'.
trimapM_ ∷ (Trifoldable f, Monad m)
         ⇒ (α → m χ) → (β → m ψ) → (γ → m ω) → f α β γ → m ()
trimapM_ = tritraverse_

-- | Perform all actions contained in the structure, but ignore the values.
trisequenceA_ ∷ (Trifoldable f, Applicative g) ⇒ f (g α) (g β) (g γ) → g ()
trisequenceA_ = tritraverse_ id id id

-- | Same as 'trisequenceA_'.
trisequence_ ∷ (Trifoldable f, Monad m) ⇒ f (m α) (m β) (m γ) → m ()
trisequence_ = trisequenceA_


-- | Put all elements into a list.
triList ∷ Trifoldable f ⇒ f α α α → [α]
triList t = appEndo (trifoldMap cons cons cons t) []
  where cons x = Endo (x :)


-- | Whether any of the elements satisfy the given predicates.
triany ∷ Trifoldable f
       ⇒ (α → Bool) → (β → Bool) → (γ → Bool) → f α β γ → Bool
triany f g h = getAny . trifoldMap (Any . f) (Any . g) (Any . h)

-- | Whether all of the elements satisfy the given predicates.
triall ∷ Trifoldable f
       ⇒ (α → Bool) → (β → Bool) → (γ → Bool) → f α β γ → Bool
triall f g h = getAll . trifoldMap (All . f) (All . g) (All . h)


-- | The sum of all contained elements.
trisum ∷ (Trifoldable f, Num α) ⇒ f α α α → α
trisum = getSum . trifoldMap Sum Sum Sum

-- | The product of all contained elements.
triproduct ∷ (Trifoldable f, Num α) ⇒ f α α α → α
triproduct = getProduct . trifoldMap Product Product Product
