import HW1types
    ( BBox, Shape(..), Length, Point, Number, Graph, Node, Bag, norm )

main:: IO ()
main = return ()

-------------------------------- EXERCISE 1 --------------------------------

-------------------- PART A -------------------- 
-- Insert to multiset
ins :: Eq a => a -> Bag a -> Bag a
-- End of list, make new entry
ins x [] = [(x, 1)]
-- Add to existing entry
ins x (y:ys)  | x == fst y      = (x, snd y + 1):ys
              -- check next index
              | x /= fst y      = y:(ins x (ys)) -- Reducable.


-------------------- PART B -------------------- 
-- Delete from multiset
del :: Eq a => a -> Bag a -> Bag a
-- End of list (no entry found)
del x [] = []
-- Base Case (Remove 1 from entry)
del x (y:ys)  | x == fst y && snd y > 1      = (x, snd y - 1):ys
              -- Only 1 entry (delete)
              | x == fst y && snd y == 1     = ys
              -- Otherwise check next index
              | x /= fst y                   = y:del x ys

-------------------- PART C -------------------- 
-- Create a multiset
bag :: Eq a => [a] -> Bag a
-- Base case
bag [] = []
-- recursively add to the bag
bag (x:xs) = ins x (bag xs)

-------------------- PART D -------------------- 
-- check a bag for at least Z amount of Ys
checkfor :: Eq a => Bag a -> a -> Int -> Bool
checkfor [] _ _ = False
checkfor (x:xs) y z | fst x == y && snd x >= z   = True
                    | otherwise                  = checkfor xs y z

-- Find if a subbag exists
subbag :: Eq a => Bag a -> Bag a -> Bool
-- subbag [] [] = True
subbag [] _ = True
-- if Y does not contain at least enough of X, then the subbag is false
subbag (x:xs) (y:ys)  | checkfor (y:ys) (fst x) (snd x) == True = subbag xs (y:ys)
                      | otherwise                               = False

-------------------- PART E --------------------
-- Check if elements only exist once
isSet :: Eq a => Bag a -> Bool
isSet [] = True
isSet (x:xs) | snd x == 1 = isSet (xs)
             | otherwise  = False

-------------------- PART F --------------------
-- recursively add the second numbers
size :: Bag a -> Int
size [] = 0
size (x:xs) = snd x + size (xs)


-------------------------------- EXERCISE 2 --------------------------------

-------------------- PART A --------------------
-- get all the nodes
nodes :: Graph -> [Node]
nodes [] = []
nodes (x:xs) = norm (fst x : snd x : nodes (xs))

-------------------- PART B --------------------
suc :: Node -> Graph -> [Node]
suc _ [] = []
-- The second element of every node
-- if the first matches X
suc x (y:ys) | fst y == x   = snd y : suc x ys
             | otherwise    = suc x ys

-------------------- PART C --------------------
detach :: Int -> Graph -> Graph
detach _ [] = []
-- If the edge contains mention of X, don't append to the new Graph
-- Otherwise continue on
detach x (y:ys) | fst y == x || snd y == x  = detach x ys
                | otherwise                 = y : detach x ys

-------------------- PART D --------------------
cyc :: Int -> Graph
cyc 0 = []
--cyc 1 = [(1, 1)]
cyc x = [ (i,i+1) | i <- [1..x-1]] ++ [(x, 1)]


-------------------------------- EXERCISE 3 --------------------------------

-------------------- PART A --------------------
-- Return the corresponding width formula
width :: Shape -> Length
width (Pt p) = 0
width (Circle p l) = 2*l
width (Rect p w h) = w

-------------------- PART B --------------------
-- Return the corresponding value sets
bbox :: Shape -> BBox
bbox (Pt p) = (p, p)
bbox (Circle p r) = ((fst p - r, snd p - r), (fst p + r, snd p + r))
bbox (Rect p w h) = ((fst p, snd p), (fst p + w, snd p + h))

-------------------- PART C --------------------
-- Find the minimum X coordinate

minX :: Shape -> Number
minX (Pt p) = fst p
minX (Circle p r) = fst p - r
minX (Rect p w h) | fst p >= fst p + w   = fst p + w
                  | fst p < fst p + w   = fst p

-------------------- PART D --------------------
-- Add points together
addPt :: Point -> Point -> Point
addPt p1 p2 = (fst p1 + fst p2, snd p1 + snd p2)

-- Move a shape by adding the X and Y values of the points
move :: Shape -> Point -> Shape
move (Pt p) p2 = (Pt (addPt p p2))
move (Circle p r) p2 = (Circle (addPt p p2) r)
move (Rect p w h) p2 = (Rect (addPt p p2) w h)