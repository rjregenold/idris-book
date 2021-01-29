module Chapter4

data Direction
  = North
  | East
  | South
  | West

turnClockwise : Direction -> Direction
turnClockwise North = East
turnClockwise East = South
turnClockwise South = West
turnClockwise West = North

||| Represents shapes
data Shape
  = ||| A triangle, with its base and height
    Triangle Double Double
  | ||| A rectangle, with its length and height
    Rectangle Double Double
  | ||| A circle, with its radius
    Circle Double

%name Shape shape, shape1, shape2

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Picture
  = Primitive Shape
  | Combine Picture Picture
  | Rotate Double Picture
  | Translate Double Double Picture

%name Picture pic, pic1, pic2

rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 10 10)

testPicture : Picture
testPicture 
  = Combine (Translate 5 5 rectangle)
    (Combine (Translate 35 5 circle) 
    (Translate 15 25 triangle))

pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic pic1) = pictureArea pic + pictureArea pic1
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic

data Biggest = NoTriangle | Size Double

compareBiggest : Biggest -> Biggest -> Biggest
compareBiggest NoTriangle NoTriangle = NoTriangle
compareBiggest NoTriangle s@(Size x) = s
compareBiggest s@(Size x) NoTriangle = s
compareBiggest a@(Size x) b@(Size y) = if x > y then a else b

biggestTriangle : Picture -> Biggest
biggestTriangle (Primitive tri@(Triangle x y)) = Size (area tri)
biggestTriangle (Primitive (Rectangle x y)) = NoTriangle
biggestTriangle (Primitive (Circle x)) = NoTriangle
biggestTriangle (Combine pic pic1) = 
  let x0 = biggestTriangle pic
      x1 = biggestTriangle pic1
   in compareBiggest x0 x1
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic

data DivResult = DivByZero | Result Double
safeDivide : Double -> Double -> DivResult
safeDivide x y = if y == 0
                    then DivByZero
                    else Result (x / y)

safeDivide' : Double -> Double -> Maybe Double
safeDivide' x y = if y == 0
                     then Nothing
                     else Just (x / y)

data Tree elem
  = Empty
  | Node (Tree elem) elem (Tree elem)

%name Tree tree, tree1

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) = case compare x val of
                                          LT => Node (insert x left) val right
                                          EQ => orig
                                          GT => Node left val (insert x right)

data BSTree : Type -> Type where
  Empty' : Ord elem => BSTree elem
  Node' : Ord elem => (left : BSTree elem) -> (val : elem) ->
                     (right : BSTree elem) -> BSTree elem

%name BSTree tree, tree1

insert' : elem -> BSTree elem -> BSTree elem
insert' x Empty' = Node' Empty' x Empty'
insert' x t@(Node' left val right) = case compare x val of
                                        LT => Node' (insert' x left) val right
                                        EQ => t
                                        GT => Node' left val (insert' x right)

