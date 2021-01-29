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
