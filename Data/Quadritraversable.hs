module Data.Quadritraversable
  (module Data.Quadrifunctor,
   module Data.Quadrifoldable,
   Quadritraversable (..),
   quadmapDefault, quadfoldMapDefault,
   quadfor, quadforM)
where

import Data.Quadrifunctor
import Data.Quadrifoldable
import Data.Functor.Identity
import Control.Applicative


-- | Structures which can be traversed in order while keeping the shape, like
-- 'Traversable'.
class (Quadrifunctor f, Quadrifoldable f) ⇒ Quadritraversable f where
    {-# MINIMAL quadtraverse | quadsequenceA #-}
    -- | Runs the effectful actions at each element and collect the results in
    -- a new structure of the same shape as the old.
    quadtraverse ∷ Applicative g
                 ⇒ (α → g α') → (β → g β') → (γ → g γ') → (δ -> g δ')
                 → f α β γ δ → g (f α' β' γ' δ')
    -- | Runs all the contained actions and collects the results in a structure
    -- of the same shape.
    quadsequenceA ∷ Applicative g ⇒ f (g α) (g β) (g γ) (g δ) → g (f α β γ δ)
    -- | Same as 'quadtraverse'.
    quadmapM ∷ Monad m
             ⇒ (α → m α') → (β → m β') → (γ → m γ') → (δ → m δ')
             → f α β γ δ → m (f α' β' γ' δ')
    quadmapM = quadtraverse
    -- | Same as 'quadsequenceA'.
    quadsequence ∷ Monad m ⇒ f (m α) (m β) (m γ) (m δ) → m (f α β γ δ)
    quadsequence = quadsequenceA

    quadtraverse f g h k = quadsequenceA . quadmap f g h k
    quadsequenceA = quadtraverse id id id id


instance Quadritraversable (,,,) where
    quadtraverse f g h k (x, y, z, w) = (,,,) <$> f x <*> g y <*> h z <*> k w


-- | Suitable default definition for 'quadmap'.
quadmapDefault ∷ Quadritraversable f
               ⇒ (α → α') → (β → β') → (γ → γ') → (δ → δ')
               → f α β γ δ → f α' β' γ' δ'
quadmapDefault f g h k =
    runIdentity . quadtraverse (id' f) (id' g) (id' h) (id' k)
  where id' r = Identity . r


-- | Suitable default definition for 'quadfoldMap'.
quadfoldMapDefault ∷ (Quadritraversable f, Monoid m)
                   ⇒ (α → m) → (β → m) → (γ → m) → (δ → m)
                   → f α β γ δ → m
quadfoldMapDefault f g h k = getConst . quadtraverse (c f) (c g) (c h) (c k)
  where c r = Const . r


-- | Same as 'quadtraverse' but with the argument order swapped.
quadfor ∷ (Quadritraversable f, Applicative g)
        ⇒ f α β γ δ → (α → g α') → (β → g β') → (γ → g γ') → (δ → g δ')
        → g (f α' β' γ' δ')
quadfor t f g h k = quadtraverse f g h k t

-- | Same as 'quadmapM' but with the argument order swapped.
quadforM ∷ (Quadritraversable f, Monad m)
         ⇒ f α β γ δ → (α → m α') → (β → m β') → (γ → m γ') → (δ → m δ')
         → m (f α' β' γ' δ')
quadforM t f g h k = quadmapM f g h k t
