module Data.Quadrifoldable
  (Quadrifoldable (..),
   quadfoldr', quadfoldrM,
   quadfoldl', quadfoldlM,
   quadtraverse_, quadmapM_,
   quadsequenceA_, quadsequence_,
   quadList, quadany, quadall, quadsum, quadproduct)
where

import Data.Monoid


-- | Foldable structures (like 'Foldable') but with four types of elements.
class Quadrifoldable f where
    {-# MINIMAL quadfoldMap | quadfoldr #-}
    -- | Combine all elements together.
    quadfold    ∷ Monoid m ⇒ f m m m m → m
    -- | Map all elements to a common monoidal type and combine the results.
    quadfoldMap ∷ Monoid m ⇒ (α → m) → (β → m) → (γ → m) → (δ → m)
                → f α β γ δ → m
    -- | Combine elements in a right-associative way, like 'foldr'.
    quadfoldr   ∷ (α → ε → ε) → (β → ε → ε) → (γ → ε → ε) → (δ → ε → ε) → ε
                → f α β γ δ → ε
    -- | Combine elements in a left-associative way, like 'foldl'.
    quadfoldl   ∷ (ε → α → ε) → (ε → β → ε) → (ε → γ → ε) → (ε → δ → ε) → ε
                → f α β γ δ → ε

    -- stolen from bifoldable, obv
    quadfold = quadfoldMap id id id id
    quadfoldMap f g h k =
        quadfoldr (mappend . f) (mappend . g) (mappend . h) (mappend . k) mempty
    quadfoldr f g h k z t =
        appEndo (quadfoldMap (Endo . f) (Endo . g) (Endo . h) (Endo . k) t) z
    quadfoldl f g h k z t =
        appEndo (getDual (quadfoldMap (dual f) (dual g) (dual h) (dual k) t)) z
      where dual r = Dual . Endo . flip r


instance Quadrifoldable (,,,) where
    quadfoldMap f g h k (x, y, z, w) = f x <> g y <> h z <> k w


-- | Strict version of 'quadfoldr'.
quadfoldr' ∷ Quadrifoldable f
           ⇒ (α → ε → ε) → (β → ε → ε) → (γ → ε → ε) → (δ → ε → ε) → ε
           → f α β γ δ → ε
quadfoldr' f g h k z₀ t = quadfoldl (s f) (s g) (s h) (s k) id t z₀
  where s r ℓ x z = ℓ $! r x z

-- | Strict version of 'quadfoldl'.
quadfoldl' ∷ Quadrifoldable f
           ⇒ (ε → α → ε) → (ε → β → ε) → (ε → γ → ε) → (ε → δ → ε) → ε
           → f α β γ δ → ε
quadfoldl' f g h k z₀ t = quadfoldr (s f) (s g) (s h) (s k) id t z₀
  where s r x ℓ z = ℓ $! r z x


-- | Monadic version of 'quadfoldr'.
quadfoldrM ∷ (Quadrifoldable f, Monad m)
           ⇒ (α → ε → m ε) → (β → ε → m ε) → (γ → ε → m ε) → (δ → ε → m ε) → ε
           → f α β γ δ → m ε
quadfoldrM f g h k z₀ t = quadfoldl (m f) (m g) (m h) (m k) return t z₀
  where m r ℓ x z = r x z >>= ℓ

-- | Monadic version of 'quadfoldl'.
quadfoldlM ∷ (Quadrifoldable f, Monad m)
           ⇒ (ε → α → m ε) → (ε → β → m ε) → (ε → γ → m ε) → (ε → δ → m ε) → ε
           → f α β γ δ → m ε
quadfoldlM f g h k z₀ t = quadfoldr (m f) (m g) (m h) (m k) return t z₀
  where m r x ℓ z = r z x >>= ℓ


-- | Perform some effectful actions to all elements left-to-right, and ignore
-- the results.
quadtraverse_ ∷ (Quadrifoldable f, Applicative g)
              ⇒ (α → g χ) → (β → g ψ) → (γ → g ω) → (δ → g φ)
              → f α β γ δ → g ()
quadtraverse_ f g h k = quadfoldr (m f) (m g) (m h) (m k) (pure ())
  where m r x y = r x *> y

-- | Same as 'quadtraverse_'.
quadmapM_ ∷ (Quadrifoldable f, Monad m)
          ⇒ (α → m χ) → (β → m ψ) → (γ → m ω) → (δ → m φ) → f α β γ δ → m ()
quadmapM_ = quadtraverse_

-- | Perform all actions contained in the structure, but ignore the values.
quadsequenceA_ ∷ (Quadrifoldable f, Applicative g)
               ⇒ f (g α) (g β) (g γ) (g δ) → g ()
quadsequenceA_ = quadtraverse_ id id id id

-- | Same as 'quadsequenceA_'.
quadsequence_ ∷ (Quadrifoldable f, Monad m)
              ⇒ f (m α) (m β) (m γ) (m δ) → m ()
quadsequence_ = quadsequenceA_


-- | Put all elements into a list.
quadList ∷ Quadrifoldable f ⇒ f α α α α → [α]
quadList t = appEndo (quadfoldMap cons cons cons cons t) []
  where cons x = Endo (x :)


-- | Whether any of the elements satisfy the given predicates.
quadany ∷ Quadrifoldable f
        ⇒ (α → Bool) → (β → Bool) → (γ → Bool) → (δ → Bool) → f α β γ δ → Bool
quadany f g h k = getAny . quadfoldMap (Any . f) (Any . g) (Any . h) (Any . k)

-- | Whether all of the elements satisfy the given predicates.
quadall ∷ Quadrifoldable f
        ⇒ (α → Bool) → (β → Bool) → (γ → Bool) → (δ → Bool) → f α β γ δ → Bool
quadall f g h k = getAll . quadfoldMap (All . f) (All . g) (All . h) (All . k)


-- | The sum of all contained elements.
quadsum ∷ (Quadrifoldable f, Num α) ⇒ f α α α α → α
quadsum = getSum . quadfoldMap Sum Sum Sum Sum

-- | The product of all contained elements.
quadproduct ∷ (Quadrifoldable f, Num α) ⇒ f α α α α → α
quadproduct = getProduct . quadfoldMap Product Product Product Product
