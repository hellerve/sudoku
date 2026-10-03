{-# OPTIONS --guardedness #-}
module sudoku where

open import Data.Bool using (Bool; true; false; not; _∨_)
open import Data.Bool.ListAction using (any)
open import Data.Fin using (Fin; toℕ)
open import Data.List using (List; []; _∷_; map; concatMap; filterᵇ; upTo; length)
open import Data.List.Relation.Unary.All as LA using ()
open import Data.List.Relation.Unary.Unique.Propositional using (Unique)
open import Data.Maybe using (Maybe; just; nothing)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _/_; _%_; _≟_; _≡ᵇ_; _<ᵇ_; _≤_; _≤?_)
open import Data.Nat.Show using (show)
open import Data.Product using (Σ; _×_; _,_)
open import Data.String using (String; unwords; unlines)
open import Data.Sum using (_⊎_)
open import Data.Vec as V using (Vec; lookup; _[_]≔_; allFin; fromList; toList)
open import Data.Vec.Relation.Binary.Pointwise.Inductive as VP using (Pointwise)
open import Data.Vec.Relation.Unary.All as VA using ()
open import Function using (_∘_)
open import IO using (Main; run; putStrLn)
open import Relation.Binary.PropositionalEquality using (_≡_)
open import Relation.Nullary using (Dec; yes; no)
open import Relation.Nullary.Decidable using (_×?_; _⊎?_)

open import Data.List.Relation.Unary.Unique.DecPropositional _≟_ using (unique?)

Board : Set
Board = Vec ℕ 81

Cell : Set
Cell = Fin 81

cells : List Cell
cells = toList (allFin 81)

row col box : Cell → ℕ
row i = toℕ i / 9
col i = toℕ i % 9
box i = (toℕ i / 27) * 3 + (toℕ i % 9) / 3

unit : (Cell → ℕ) → ℕ → List Cell
unit f k = filterᵇ (λ i → f i ≡ᵇ k) cells

units : List (List Cell)
units = concatMap (λ f → map (unit f) (upTo 9)) (row ∷ col ∷ box ∷ [])

Digit : ℕ → Set
Digit v = 1 ≤ v × v ≤ 9

Keeps : ℕ → ℕ → Set
Keeps given v = given ≡ 0 ⊎ given ≡ v

Solution : Board → Board → Set
Solution p s = Pointwise Keeps p s
             × VA.All Digit s
             × LA.All (λ u → Unique (map (lookup s) u)) units

solution? : (p s : Board) → Dec (Solution p s)
solution? p s = VP.decidable (λ g v → (g ≟ 0) ⊎? (g ≟ v)) p s
         ×? VA.all? (λ v → (1 ≤? v) ×? (v ≤? 9)) s
         ×? LA.all? (λ u → unique? (map (lookup s) u)) units

peer : Cell → Cell → Bool
peer i j = (row i ≡ᵇ row j) ∨ (col i ≡ᵇ col j) ∨ (box i ≡ᵇ box j)

peers : Vec (List Cell) 81
peers = V.map (λ i → filterᵇ (peer i) cells) (allFin 81)

candidates : Board → Cell → List ℕ
candidates b i = filterᵇ (λ d → not (any (_≡ᵇ d) used)) (map suc (upTo 9))
  where used = map (lookup b) (lookup peers i)

empties : Board → List Cell
empties b = filterᵇ (λ i → lookup b i ≡ᵇ 0) cells

holes : Board → ℕ
holes = length ∘ empties

mrv : Board → List Cell → Maybe (Cell × List ℕ)
mrv b []       = nothing
mrv b (i ∷ is) = pick (candidates b i) (mrv b is)
  where
    pick : List ℕ → Maybe (Cell × List ℕ) → Maybe (Cell × List ℕ)
    pick cs nothing        = just (i , cs)
    pick cs (just (j , ds)) with length cs <ᵇ suc (length ds)
    ... | true  = just (i , cs)
    ... | false = just (j , ds)

search : ℕ → Board → Maybe Board
branch : ℕ → Board → Cell → List ℕ → Maybe Board

search zero    b = just b
search (suc n) b with mrv b (empties b)
... | nothing       = just b
... | just (i , ds) = branch n b i ds

branch n b i []       = nothing
branch n b i (d ∷ ds) with search n (b [ i ]≔ d)
... | just s  = just s
... | nothing = branch n b i ds

solve : (p : Board) → Maybe (Σ Board (Solution p))
solve p with search (holes p) p
... | nothing = nothing
... | just s with solution? p s
...   | yes ok = just (s , ok)
...   | no  _  = nothing

render : Board → String
render s = unlines (map (λ r → unwords (map (show ∘ lookup s) (unit row r))) (upTo 9))

puzzle : Board
puzzle = fromList
  ( 8 ∷ 0 ∷ 0 ∷ 0 ∷ 0 ∷ 0 ∷ 0 ∷ 0 ∷ 0
  ∷ 0 ∷ 0 ∷ 3 ∷ 6 ∷ 0 ∷ 0 ∷ 0 ∷ 0 ∷ 0
  ∷ 0 ∷ 7 ∷ 0 ∷ 0 ∷ 9 ∷ 0 ∷ 2 ∷ 0 ∷ 0
  ∷ 0 ∷ 5 ∷ 0 ∷ 0 ∷ 0 ∷ 7 ∷ 0 ∷ 0 ∷ 0
  ∷ 0 ∷ 0 ∷ 0 ∷ 0 ∷ 4 ∷ 5 ∷ 7 ∷ 0 ∷ 0
  ∷ 0 ∷ 0 ∷ 0 ∷ 1 ∷ 0 ∷ 0 ∷ 0 ∷ 3 ∷ 0
  ∷ 0 ∷ 0 ∷ 1 ∷ 0 ∷ 0 ∷ 0 ∷ 0 ∷ 6 ∷ 8
  ∷ 0 ∷ 0 ∷ 8 ∷ 5 ∷ 0 ∷ 0 ∷ 0 ∷ 1 ∷ 0
  ∷ 0 ∷ 9 ∷ 0 ∷ 0 ∷ 0 ∷ 0 ∷ 4 ∷ 0 ∷ 0 ∷ [])

report : Maybe (Σ Board (Solution puzzle)) → String
report nothing        = "unsatisfiable"
report (just (s , _)) = render s

main : Main
main = run (putStrLn (report (solve puzzle)))
