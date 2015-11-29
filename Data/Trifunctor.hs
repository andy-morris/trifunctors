{-# LANGUAGE DeriveGeneric, DeriveTraversable, TemplateHaskell #-}
module Data.Trifunctor
  (Trifunctor (..))
where

class Trifunctor f where
    {-# MINIMAL trimap | (first3, second3, third3) #-}
    trimap  ∷ (α → α') → (β → β') → (γ → γ') → f α β γ → f α' β' γ'
    first3  ∷ (α → α') → f α β γ → f α' β  γ
    second3 ∷ (β → β') → f α β γ → f α  β' γ
    third3  ∷ (γ → γ') → f α β γ → f α  β  γ'

    trimap f g h = first3 f . second3 g . third3 h
    first3  f = trimap f  id id
    second3 g = trimap id g  id
    third3  h = trimap id id h

{-# RULES
"trimap/trimap"
    ∀f₁ f₂ g₁ g₂ h₁ h₂ t.
    trimap f₁ g₁ h₁ (trimap f₂ g₂ h₂ t) =
    trimap (f₁ . f₂) (g₁ . g₂) (h₁ . h₂) t
"trimap/first3"
    ∀f f' g h t.
    trimap f g h (first3 f' t) =
    trimap (f . f') g h t
"trimap/second3"
    ∀f g g' h t.
    trimap f g h (second3 g' t) =
    trimap f (g . g') h t
"trimap/third3"
    ∀f g h h' t.
    trimap f g h (third3 h' t) =
    trimap f g (h . h') t
"first3/trimap"
    ∀f f' g h t.
    first3 f' (trimap f g h t) =
    trimap (f' . f) g h t
"second3/trimap"
    ∀f g g' h t.
    second3 g' (trimap f g h t) =
    trimap f (g' . g) h t
"third3/trimap"
    ∀f g h h' t.
    third3 h' (trimap f g h t) =
    trimap f g (h' . h) t
  #-}


instance Trifunctor (,,) where trimap f g h (x, y, z) = (f x, g y, h z)

