module LeqTst where

open import IO.Primitive
open import Function
open import Data.Char using (toNat)
open import Data.List using (List; []; _∷_)
open import Coinduction
open import Data.Colist using (Colist; []; _∷_)
open import Data.Sum
open import Data.String hiding (toList)
open import Data.Nat hiding (_≤?_)
open import Data.Product
open import Foreign.Haskell

{-# NO_TERMINATION_CHECK #-}
toList : {a : Set} → Colist a → List a
toList [] = []
toList (x ∷ xs) = x ∷ toList (♭ xs)

readℕ×ℕ : Costring → ℕ × ℕ
readℕ×ℕ cs with toList cs
... | (n ∷ ' ' ∷ m ∷ _) = toNat n ∸ toNat '0' , toNat m ∸ toNat '0'
... | _ = 0 , 0

infixl 4 _<$>_
_<$>_ : {a b : Set} → (a → b) → IO a → IO b
f <$> m = m >>= λ a →
          return (f a)

showLeq : ∀ {n m} → n ≤ m → String
showLeq z≤n = "z≤n"
showLeq (s≤s p) = "(s≤s " ++ showLeq p ++ ")"

showOrLeq : ∀ {n m} → n ≤ m ⊎ n > m → String
showOrLeq (inj₁ p) = showLeq p ++ " : n ≤ m"
showOrLeq (inj₂ p) = showLeq p ++ " : n > m"

show : ∀ {n m} → n ≤ m ⊎ n > m → Costring
show = toCostring ∘ showOrLeq

_≤?_ : (n : ℕ) (m : ℕ) → n ≤ m ⊎ n > m
zero ≤? m = inj₁ z≤n
suc n ≤? zero = inj₂ $ s≤s z≤n
suc n ≤? suc m with n ≤? m
... | inj₁ p = inj₁ $ s≤s p
... | inj₂ p = inj₂ $ s≤s p

testLeq : IO Unit
testLeq =
  getContents >>= λ stdin →
  let (n , m) = readℕ×ℕ stdin
      proof : n ≤ m ⊎ n > m
      proof = n ≤? m
  in
  putStrLn $ show proof

main = testLeq
