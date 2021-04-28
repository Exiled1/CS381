module HW1types where

import Data.List (nub,sort)


--------------- Types for Exercise 1 -------------------
--

type Bag a = [(a,Int)]
lista = [1,2,2,3,3,3,4]
bag1 :: Bag Int
bag2 :: Bag Int
bag1 = [(1,1),(2,5),(3,4),(4,1)]
bag2 = [(1,2),(2,5),(3,6),(4,1),(5,1)]


-- (Part A) Create the function for the insert
ins :: Eq a => a -> Bag a -> Bag a
ins a [] = [(a,1)]
-- If element add into the list does not exist, add it with occurrence of 1
ins a ((x,n):xs) | a == x    = ((x,n + 1):xs)			-- If both the element match a value in a set, increment the occurrence
				 | otherwise = ((x,n):ins a xs)    	-- If nothing matches, insert it in the list as a new set
 

-- (Part B) Create the function for the deletion
del :: Eq a => a -> Bag a -> Bag a
del a []         = []											-- If the element meant to be delete and it is an empty list, return list
del a ((x,n):xs) 
	| a == x && n >	1 = ((x,n - 1):xs)	-- If there more than one occurrence, decrement by one
	| a == x    		  = xs					-- If there only one occurrence, remove it and return other sets in list
	| otherwise       = ((x,n):del a xs) -- If nothing matches, move to the next set in list for deletion


-- (Part C) Create a function represent the bag
bag :: Eq a => [a] -> Bag a
bag [] 	  = []													-- If the list is empty, return the empty list
bag (x:xs) = ins x (bag xs)									-- If there is a list, insert the element and move on to insert the rest


-- (Part D) Create a function to determine the subbag
subbag :: Eq a => Bag a -> Bag a -> Bool
subbag b1 b2 = b1 == checkbag b1 b2

checkbag :: Eq a => Bag a -> Bag a -> Bag a
checkbag b1 b2 = [(a,n) | (a,n) <- b1, (b,m) <- b2, a == b && n <= m]


-- (Part E) Create a function for isSet to see the bag is a set
isSet :: Eq a => Bag a -> Bool
isSet [] = True
isSet ((x,n):xs) | n > 1     = False
				| otherwise = isSet xs


-- (Part F) Create a function to find number of elements in the bag
size :: Bag a -> Int
size [] 			 = 0												-- If list is empty, return 0 for the size of the bag
size ((x,n):xs) = n + size xs								   -- If list is present, get the element occurence and add the rest of list
--
--------------------------------------------------------


-------- Types and functions for Exercise 2 ------------
--
type Node  = Int
type Edge  = (Node,Node)
type Graph = [Edge]
type Path  = [Node]

norm :: Ord a => [a] -> [a]
norm = sort . nub

g :: Graph 
g = [(1,2),(1,3),(2,3),(2,4),(3,4)]

-- (Part A) Create a list to find the nodes contain in graph
nodes :: Graph -> [Node]
nodes [] 	     = []											-- If Graph is empty, return empty node
nodes ((x,n):xs) = norm([x,n] ++ nodes xs)				-- If there is Graph, sort it using norm and concat with other items in list


-- (Part B) Create a function finding the successor of it own nodes
suc :: Node -> Graph -> [Node]
suc i []         = []											-- If the node have no succ, return empty list
suc i ((x,n):xs) | i == x     = ([n] ++ suc i xs)		-- If matches, store the succ and concat if any other nodes have matches
					  | otherwise  = suc i xs					-- If no node matches with the number given, move onto the next node


-- (Part C) Create a function to remove a node and all of it incident edges
detach :: Node -> Graph -> Graph
detach i []         = []										-- If trying to detach an empty list, return empty list
detach i ((x,n):xs) | x == i	  = detach i xs			-- If the nodes match, remove from list
					| n == i 	  = detach i xs  			-- If the edges match, remove it from list
					| otherwise = (x,n):detach i xs   -- Otherwise, if nodes/edges don't match, move on to remove other in lists


-- (Part D) Create a function that create a cycle with any given number
--cyc :: Int -> Graph


--
--------------------------------------------------------


--------------- Types for Exercise 3 -------------------
--
type Number = Int

type Point = (Number,Number)
type Length = Number

data Shape = Pt Point
           | Circle Point Length
           | Rect Point Length Length
           deriving Show

type Figure = [Shape]

type BBox = (Point,Point)
f = [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2]


-- (Part A) Compute the width of the shape
width :: Shape -> Length
width (Pt x)  		   = 0										-- If given shape Pt, the width will be 0
width (Circle x l)   = l * 2									-- If given Circle, take the length times it by 2
width (Rect x l1 l2) = l1										-- If given Rect, take the length of the first length


-- (Part B) Compute bounding box shape
bbox :: Shape -> BBox
bbox (Pt x) 				  = (x,x)										-- If given Pt, return both set
bbox (Circle (x1,x2) l)   = ((x1 - l, x2 - l),(x1 + l,x2 + l))	-- If Circle, return each set +/- the length
bbox (Rect (x1,x2) l1 l2) = ((x1,x2),(x1 + l1,x2 + l2))			-- If Rect, return ori set and set plus both length


-- (Part C) Compute the minimum coordinate of a shape
minX :: Shape -> Number
minX (Pt x) 			  = fst x
minX (Circle (x1,x2) l)   = x1 - l
minX (Rect (x1,x2) l1 l2) = x1

								  	 
-- (Part D) Create a function to move the position of the shape
move :: Shape -> Point -> Shape
move (Pt x) p         = (Pt (addPt x p))					-- If given Pt, add the new coordinate and return Pt
move (Circle x l) p   = (Circle (addPt x p) l)			-- If given Circle, do the same method as Pt
move (Rect x l1 l2) p = (Rect (addPt x p) l1 l2)		-- Do the same again for the Rect

-- Extra Function for (Part D)
addPt :: Point -> Point -> Point
addPt (x1,y1) (x2,y2) = ((x1 + x2),(y1 + y2))			-- If given two points, add them up together
--
---------------------------------------------------------
