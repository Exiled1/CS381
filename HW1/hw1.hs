import HW1types
{-
Group 38Fun:
- Rodolfo Peralta
- Tu Lam
- Mitchell Radford
- Hugh MacWilliams
-}

{-
    Btw, y in this scenario is going to be the bag declaration. (x:xs), however, x is taken by the num, I might change the variable to be num so that it makes more sense afterwards.

  Make the insert function check for a couple things. (x, n)

  1. Check the values in the bag to make sure it's unique.

  2. If the element is not unique in the multiset:

    - basically, define another variable, y (or a) and a counter (N or some other name), basically just to keep track of n

    - If the element key-pair list matches another, (if x == y), then make it add 1 to the n value (n + 1) on top of the multiset (mset/ms)

  3. The next case means that it's a unique key pair, so just insert it into the bag.
  (1a)
-}
-- (1a)
ins :: (Eq a) => a -> Bag a -> Bag a

ins num [] = [(num, 1)]
-- If the list is empty, add the first x into the list.
ins num ((x, n):mset) 
    | num == x = (x, n + 1):mset 
    | otherwise = (x, n) : ins num mset

-- (1b)
del :: (Eq a) => a -> Bag a -> Bag a
del num [] = []
del num ((x, n) : mset)
  | num == x && n == 1 = mset
  | num == x = (x, n - 1) : mset
  | otherwise = (x, n) : del num mset

-- (1c)

bag :: Eq a => [a] -> Bag a
bag = foldr ins [] -- Get all of the elements and insert them into a new bag into a blank bag

-- ins ( ins ( ins (ins ...)))


-- Note that a bag b is contained in a bag b′ if every element that occurs n times in b occurs also at least n times in b′
-- (1d helper)
(⊆) :: Eq a => Bag a -> Bag a -> Bool
(⊆) [] _ = True
(⊆) (x:xs) ys = (∈) x ys && (⊆) xs ys
-- (x:xs) ⊆ ys = x ∈ ys && xs ⊆ ys

(∈) :: Eq a => (a, Int) -> Bag a -> Bool
(∈) _ [] = False
(∈) (x, n) ((y, m):mset)
  | x == y && n <= m = True -- The current element is in the Bag
  | otherwise =  (∈) (x,n) mset  -- The current element isn't in the Bag.
{-
  (1, 1)
  (2, 1) , (1, 1)

 A ⊆ B if, for all (x,n) ∈ A
  we have (x,m >= n) ∈ B

  get list that contains all combos of both bags (cartesian product) and if the first element in the tuple are the same, the count of the Left hand tuple is less than the count of the Right hand tuple, then it's a subbag.
-}
-- subbag b1 b2 = b1 == [(x,n) | (x, n) <- b1, (y, m) <- b2, (x==y && n <= m)]
-- (1d code)
subbag :: Eq a => Bag a -> Bag a -> Bool
subbag b1 b2 = b1 == [(x,n) | (x, n) <- b1, (y, m) <- b2, x==y && n <= m] 


-- (1e)
isSet :: Eq a => Bag a -> Bool
isSet [] = True
isSet ((x, n):mset)
  | n > 1 = False
  | otherwise = isSet mset

-- (1f)
size:: Bag a -> Int
size [] = 0
size ((x,n):xs) = n + size xs

bag1 :: Bag Int
bag2 :: Bag Int

bag1 = [(2,1)]
bag2 = [(2,1),(1, 2), (4, 3)]

-- EXERCISE 2. GRAPHS BOIIII
g :: Graph 
g = [(1,2),(1,3),(2,3),(2,4),(3,4)]

h :: Graph
h = [(1,2),(1,3),(2,1),(3,2),(4,4)]

{-
type Node  = Int
type Edge  = (Node,Node)
type Graph = [Edge]
type Path  = [Node]
-}
-- (2a)
nodes :: Graph -> [Node]
nodes graph = norm([x | (x,y) <- graph] ++ [y | (x,y) <- graph])

-- (2b)
suc :: Node -> Graph -> [Node]
suc n graph = [y | (x,y) <- graph, n == x]
-- [(1,2),(1,3),(2,3),(2,4),(3,4)]



-- (2c)
detach :: Node -> Graph -> Graph
detach n graph = [(x,y) | (x,y) <- graph, n /= x && n /= y]

cyc :: Int -> Graph 
cyc n = [(x, mod x n + 1)  | x <- [1..n]]

-- EXERCISE 3, Data Types Pog
{-
type Number = Int

type Point = (Number,Number)
type Length = Number

data Shape = Pt Point
           | Circle Point Length
           | Rect Point Length Length
           deriving Show

type Figure = [Shape]

type BBox = (Point,Point)

-}
f :: [Shape]
f = [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2]
-- [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2]

-- (3a)
width :: Shape -> Length
width (Pt p) = 0
width (Circle _ l) = 2*l
width (Rect _ l _) = l

-- (3b)
bbox :: Shape -> BBox
bbox (Pt p) = (p,p)
bbox (Circle (x, y) l) = ((x - l,x - l),(y + l,y + l))
bbox (Rect (x, y) l w) = ((x, y),(x+l, y + w))


-- (3c)
minX :: Shape -> Number
minX (Pt (x,_)) = x
minX (Circle (x,_) l) = x - l
minX (Rect (x,_) _ _) = x

-- (3d)
move :: Shape -> Point -> Shape
move (Pt (x, y)) (a,b) = Pt (x + a, y + b)
move (Circle (x, y) l) (a, b) = Circle (x + a, y + b) l
move (Rect (x,y) l w) (a, b) = Rect (x + a, y + b) l w


genList::Eq a=> Bag a -> Bag a -> Bag a
genList b1 b2 = [(x,n) | (x, n) <- b1, (y, m) <- b2, x==y && n <= m]

bg1 :: [(Int, Int)]
bg1 = [(1,3), (2,1), (3,3)]

bg2 :: [(Int, Int)]
bg2 = [(1,3), (2,1), (3,3), (4,6), (5,3)]
