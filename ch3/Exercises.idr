module Exercises

import Data.Vect


myLength : List a -> Nat
myLength [] = 0
myLength (x :: xs) = 1 + myLength xs

myReverse : List a -> List a
myReverse [] = []
myReverse (x :: xs) = myReverse xs ++ [x]

myReverseTR : List a -> List a
myReverseTR [] = []
myReverseTR (x :: xs) = f xs [x]
where
  f : List a -> List a -> List a
  f [] acc = acc
  f (y :: xs) acc = f xs (y :: acc)

myMap : (a -> b) -> List a -> List b
myMap f [] = []
myMap f (x :: xs) = f x :: map f xs

myMapVect : (a -> b) -> Vect len a -> Vect len b
myMapVect f [] = []
myMapVect f (x :: xs) = f x :: myMapVect f xs

createEmpties : {n : _} -> Vect n (Vect 0 elem)
createEmpties = replicate _ []

transposeMat : {n : _} -> Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = zipWith (::) x (transposeMat xs)

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys
