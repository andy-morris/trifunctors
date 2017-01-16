module Data.Trifunctor (Trifunctor (..)) where

-- | A trifunctor is like a (covariant) 'Functor' or 'Data.Bifunctor.Bifunctor',
-- but with three arguments.
--
-- If you define 'trimap':
--
-- @
-- trimap id id id ≡ id
-- @
--
-- If you define 'first3', 'second3', and 'third3':
--
-- @
-- first3 id ≡ id
-- second3 id ≡ id
-- third3 id ≡ id
-- @
--
-- If both:
--
-- @
-- trimap f g h ≡ first3 f . second3 g . third3 h
-- @
class Trifunctor f where
    {-# MINIMAL trimap | (first3, second3, third3) #-}
    -- | Map over all three arguments at once.
    trimap  ∷ (α → α') → (β → β') → (γ → γ') → f α β γ → f α' β' γ'
    -- | Map covariantly over the first argument.
    first3  ∷ (α → α') → f α β γ → f α' β  γ
    -- | Map covariantly over the second argument.
    second3 ∷ (β → β') → f α β γ → f α  β' γ
    -- | Map covariantly over the third argument.
    third3  ∷ (γ → γ') → f α β γ → f α  β  γ'

    trimap f g h = first3 f . second3 g . third3 h
    first3  f = trimap f  id id
    second3 g = trimap id g  id
    third3  h = trimap id id h

instance Trifunctor (,,) where trimap f g h (x, y, z) = (f x, g y, h z)
