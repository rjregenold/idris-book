module Exercises

import Data.List

data Tree : Type -> Type where
  Empty : Tree elem
  Node : (left : Tree elem) -> (val : elem) ->
         (right : Tree elem) -> Tree elem

%name Tree tree0, tree1, tree2

insert : Ord a => a -> Tree a -> Tree a
insert x Empty = Node Empty x Empty
insert x t@(Node left val right) 
  = case compare x val of
      LT => Node (insert x left) val right
      EQ => t
      GT => Node left val (insert x right)

listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node left val right) 
  = treeToList left ++ [val] ++ treeToList right

data Expr
  = Val Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr

evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add x y) = evaluate x + evaluate y
evaluate (Sub x y) = evaluate x - evaluate y
evaluate (Mult x y) = evaluate x * evaluate y

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing m@(Just x) = m
maxMaybe m@(Just x) Nothing = m
maxMaybe a@(Just x) b@(Just y) 
  = case compare x y of
      LT => b
      EQ => a
      GT => a

||| Represents shapes
data Shape
  = ||| A triangle, with its base and height
    Triangle Double Double
  | ||| A rectangle, with its length and height
    Rectangle Double Double
  | ||| A circle, with its radius
    Circle Double

%name Shape shape0, shape1, shape2

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Picture
  = Primitive Shape
  | Combine Picture Picture
  | Rotate Double Picture
  | Translate Double Double Picture

%name Picture pic0, pic1, pic2

testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
                   (Primitive (Triangle 2 4))

testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
                   (Primitive (Circle 4))

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive t@(Triangle x y)) = Just (area t)
biggestTriangle (Primitive (Rectangle x y)) = Nothing
biggestTriangle (Primitive (Circle x)) = Nothing
biggestTriangle (Combine pic0 pic1) = maxMaybe (biggestTriangle pic0) (biggestTriangle pic1)
biggestTriangle (Rotate x pic0) = biggestTriangle pic0
biggestTriangle (Translate x y pic0) = biggestTriangle pic0
