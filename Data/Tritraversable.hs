module Data.Tritraversable
where

import Data.Trifunctor
import Data.Trifoldable
import Data.Functor.Identity
import Control.Applicative


class Trifoldable f ⇒ Tritraversable f where
    {-# MINIMAL tritraverse | trisequenceA #-}
    tritraverse ∷ Applicative g
                ⇒ (α → g α') → (β → g β') → (γ → g γ')
                → f α β γ → g (f α' β' γ')
    trisequenceA ∷ Applicative g ⇒ f (g α) (g β) (g γ) → g (f α β γ)
    trimapM ∷ Monad m
            ⇒ (α → m α') → (β → m β') → (γ → m γ')
            → f α β γ → m (f α' β' γ')
    trimapM     = tritraverse
    trisequence ∷ Monad m ⇒ f (m α) (m β) (m γ) → m (f α β γ)
    trisequence = trisequenceA

    tritraverse f g h = trisequenceA . trimap f g h
    trisequenceA = tritraverse id id id


instance Tritraversable (,,) where
    tritraverse f g h (x, y, z) = (,,) <$> f x <*> g y <*> h z


trimapDefault ∷ Tritraversable f
              ⇒ (α → α') → (β → β') → (γ → γ') → f α β γ → f α' β' γ'
trimapDefault f g h = runIdentity . tritraverse (id' f) (id' g) (id' h)
  where id' k = Identity . k


trifoldMapDefault ∷ (Tritraversable f, Monoid m)
                  ⇒ (α → m) → (β → m) → (γ → m) → f α β γ → m
trifoldMapDefault f g h = getConst . tritraverse (c f) (c g) (c h)
  where c k = Const . k
