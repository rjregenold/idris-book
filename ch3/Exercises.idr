module Exercises

import Data.Vect


myLength : List a -> Nat
myLength [] = 0
myLength (x :: xs) = 1 + myLength xs
myLength xs = ?myLength_rhs

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

dotProduct : Num a => Vect m a -> Vect m a -> a
dotProduct xs ys = sum (zipWith (*) xs ys)

multMatrix : Num a => {n: _} -> {p: _} -> Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix xs ys = go xs (transposeMat ys)
where
  go : {n: _} -> Vect n (Vect m a) -> Vect p (Vect m a) -> Vect n (Vect p a)
  go [] ys = []
  go (x :: xs) ys = map (dotProduct x) ys :: go xs ys
