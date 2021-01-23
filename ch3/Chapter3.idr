import Data.Vect


allLengths : List String -> List Nat
allLengths [] = []
allLengths (word :: words) = length word :: allLengths words

xor : Bool -> Bool -> Bool
xor True y = not y
xor False y = y

isEven : Nat -> Bool
isEven 0 = True
isEven (S k) = not (isEven k)

mutual
  even : Nat -> Bool
  even 0 = True
  even (S k) = odd k

  odd : Nat -> Bool
  odd 0 = False
  odd (S k) = even k

allLengthsVec : Vect len String -> Vect len Nat
allLengthsVec [] = []
allLengthsVec (word :: words) = length word :: allLengthsVec words

insert : Ord elem => (x : elem) -> (xsSorted : Vect k elem) -> Vect (S k) elem
insert x [] = [x]
insert x (y :: xs) = if x < y then x :: y :: xs
                              else y :: insert x xs

insSort : Ord elem => Vect len elem -> Vect len elem
insSort [] = []
insSort (x :: xs) = let xsSorted = insSort xs in
                        insert x xsSorted
