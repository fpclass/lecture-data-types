--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Data types & type aliases                                         --
--------------------------------------------------------------------------------

module Lecture where

import Prelude hiding (Maybe(..), filter)

--------------------------------------------------------------------------------
-- Booleans

-- The definition of Bool is commented out because it
-- messes with other parts of this module which rely
-- on Haskell's Bool type from the standard library.

{-
data Bool = True | False
    deriving (Eq, Show)

not :: Bool -> Bool
not True  = False
not False = True
-}

--------------------------------------------------------------------------------
-- Examples of data types

data Module = CS142 | CS118 | CS141

data Language = PHP | JavaScript | CSharp | Haskell

data Unit = Unit

data Void

--------------------------------------------------------------------------------
-- Shapes

data Shape = Rect Double Double | Circle Double

square :: Double -> Shape
square x = Rect x x

area :: Shape -> Double
area (Rect w h) = w * h
area (Circle r) = pi * r^2

isLine :: Shape -> Bool
isLine (Rect 0 h) = True
isLine (Rect w 0) = True
isLine _          = False

--------------------------------------------------------------------------------
-- Maybe

data Maybe a = Nothing | Just a

safediv :: Int -> Int -> Maybe Int
safediv x 0 = Nothing
safediv x y = Just (x `div` y)

--------------------------------------------------------------------------------
-- Natural numbers

data Nat = Zero | Succ Nat

one :: Nat
one = Succ Zero

two :: Nat
two = Succ one

three :: Nat
three = Succ two

add :: Nat -> Nat -> Nat
add Zero     m = m
add (Succ n) m = Succ (add n m)

mul :: Nat -> Nat -> Nat
mul Zero     m = Zero
mul (Succ n) m = add m (mul n m)

--------------------------------------------------------------------------------
-- Binary trees

data BinTree a = Leaf a
               | Node (BinTree a) (BinTree a)

flatten :: BinTree a -> [a]
flatten (Leaf x)   = [x]
flatten (Node l r) = flatten l ++ flatten r

depth :: BinTree a -> Int
depth (Leaf _)   = 1
depth (Node l r) = 1 + max (depth l) (depth r)

--------------------------------------------------------------------------------
-- Type aliases

type String = [Char]

type Predicate a = a -> Bool

-- `Predicate Int` is just an alias for `Int -> Bool`
isEven :: Predicate Int 
isEven n = n `mod` 2 == 0

-- `Predicate a` is just an alias for `a -> Bool`
isEven' :: (Eq a, Integral a) => Predicate a
isEven' n = n `mod` 2 == 0

--------------------------------------------------------------------------------
