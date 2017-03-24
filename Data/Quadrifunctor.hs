module Data.Quadrifunctor (Quadrifunctor (..)) where

-- | A quadrifunctor is like a (covariant) 'Functor' or
-- 'Data.Bifunctor.Bifunctor', but with four arguments.
--
-- If you define 'quadmap':
--
-- @
-- quadmap id id id id ≡ id
-- @
--
-- If you define 'first4', 'second4', 'third4', and 'fourth4':
--
-- @
-- first4 id ≡ id
-- second4 id ≡ id
-- third4 id ≡ id
-- fourth4 id ≡ id
-- @
--
-- If both:
--
-- @
-- quadmap f g h k ≡ first4 f . second4 g . third4 h . fourth4 k
-- @
class Quadrifunctor f where
    {-# MINIMAL quadmap | (first4, second4, third4, fourth4) #-}
    -- | Map over all three arguments at once.
    quadmap ∷ (α → α') → (β → β') → (γ → γ') → (δ → δ')
            → f α β γ δ → f α' β' γ' δ'
    -- | Map covariantly over the first argument.
    first4  ∷ (α → α') → f α β γ δ → f α' β  γ  δ
    -- | Map covariantly over the second argument.
    second4 ∷ (β → β') → f α β γ δ → f α  β' γ  δ
    -- | Map covariantly over the third argument.
    third4  ∷ (γ → γ') → f α β γ δ → f α  β  γ' δ
    -- | Map covariantly over the third argument.
    fourth4 ∷ (δ → δ') → f α β γ δ → f α  β  γ  δ'

    quadmap f g h k = first4 f . second4 g . third4 h . fourth4 k
    first4  f = quadmap f  id id id
    second4 g = quadmap id g  id id
    third4  h = quadmap id id h  id
    fourth4 k = quadmap id id id k

instance Quadrifunctor (,,,) where
    quadmap f g h k (x, y, z, w) = (f x, g y, h z, k w)
