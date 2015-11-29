module Data.Trifoldable
where

import Data.Monoid
import Data.Trifunctor


class Trifunctor f ⇒ Trifoldable f where
    {-# MINIMAL trifoldMap | trifoldr #-}
    trifold    ∷ Monoid m ⇒ f m m m → m
    trifoldMap ∷ Monoid m ⇒ (α → m) → (β → m) → (γ → m) → f α β γ → m
    trifoldr   ∷ (α → δ → δ) → (β → δ → δ) → (γ → δ → δ) → δ → f α β γ → δ
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
