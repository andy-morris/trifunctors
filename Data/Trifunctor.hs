{-# LANGUAGE DeriveGeneric, DeriveTraversable, TemplateHaskell #-}
module Data.Trifunctor
where

import GHC.Generics
import Data.Bifunctor
import Data.Bifunctor.TH

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
    ∀f₁ f₂ g₁ g₂ h₁ h₂.
    trimap f₁ g₁ h₁ . trimap f₂ g₂ h₂ =
    trimap (f₁ . f₂) (g₁ . g₂) (h₁ . h₂)
"trimap/first3"
    ∀f f' g h.
    trimap f g h . first3 f' =
    trimap (f . f') g h
"trimap/second3"
    ∀f g g' h.
    trimap f g h . second3 g' =
    trimap f (g . g') h
"trimap/third3"
    ∀f g h h'.
    trimap f g h . third3 h' =
    trimap f g (h . h')
"first3/trimap"
    ∀f f' g h.
    first3 f' . trimap f g h =
    trimap (f' . f) g h
"second3/trimap"
    ∀f g g' h.
    second3 g' . trimap f g h =
    trimap f (g' . g) h
"third3/trimap"
    ∀f g h h'.
    third3 h' . trimap f g h =
    trimap f g (h' . h)
  #-}


instance Trifunctor (,,) where trimap f g h (x, y, z) = (f x, g y, h z)


data Either3 α β γ = Left3 α | Mid3 β | Right3 γ
  deriving (Eq, Ord, Show, Read, Generic,
            Functor, Foldable, Traversable)

deriveBifunctor     ''Either3
deriveBifoldable    ''Either3
deriveBitraversable ''Either3

instance Trifunctor Either3 where
    trimap f _ _ (Left3  x) = Left3  $ f x
    trimap _ g _ (Mid3   y) = Mid3   $ g y
    trimap _ _ h (Right3 z) = Right3 $ h z


newtype RotL f β γ α = RotL {unRotL ∷ f α β γ}
  deriving (Eq, Ord, Show, Read, Generic)

instance Trifunctor f ⇒ Functor (RotL f β γ) where
    fmap f = RotL . first3 f . unRotL

instance Trifunctor f ⇒ Bifunctor (RotL f β) where
    bimap h f = RotL . trimap f id h . unRotL

instance Trifunctor f ⇒ Trifunctor (RotL f) where
    trimap g h f = RotL . trimap f g h . unRotL


newtype RotR f γ α β = RotR {unRotR ∷ f α β γ}
  deriving (Eq, Ord, Show, Read, Generic)

instance Trifunctor f ⇒ Functor (RotR f γ α) where
    fmap g = RotR . second3 g . unRotR

instance Trifunctor f ⇒ Bifunctor (RotR f γ) where
    bimap f g = RotR . trimap f g id . unRotR

instance Trifunctor f ⇒ Trifunctor (RotR f) where
    trimap h f g = RotR . trimap f g h . unRotR
