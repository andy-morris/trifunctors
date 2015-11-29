module Data.Tritraversable
  (module Data.Trifoldable,
   Tritraversable (..),
   trimapDefault, trifoldMapDefault,
   trifor, triforM)
where

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


trifor ∷ (Tritraversable f, Applicative g)
       ⇒ f α β γ → (α → g α') → (β → g β') → (γ → g γ') → g (f α' β' γ')
trifor t f g h = tritraverse f g h t

triforM ∷ (Tritraversable f, Monad m)
        ⇒ f α β γ → (α → m α') → (β → m β') → (γ → m γ') → m (f α' β' γ')
triforM t f g h = trimapM f g h t
