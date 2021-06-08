--Exercise 1
--We all worked together to complete this assignment. We picked the best bits and cleaned them up and put them in one document.

type Prog = [Cmd]
data Cmd = LD Int | ADD | MULT | DUP | INC | SWAP | POP Int
type Rank = Int
type CmdRank = (Int,Int)
type Stack = [Int]

--a.
rankC :: Cmd -> CmdRank
rankC (LD i)  = (0,1)
rankC ADD     = (2,1)
rankC MULT    = (2,1)
rankC DUP     = (1,2)
rankC INC     = (1,1)
rankC SWAP    = (2,2)
rankC (POP x) = (x,0) 

rankP :: Prog -> Maybe Rank
rankP c = rank c 0

rank :: Prog -> Rank -> Maybe Rank
rank [] r     =  Just r  
rank (c:cs) r | r < x     = Nothing
               | otherwise = rank cs $ r + change
              where (x,y) = rankC c
                    change = y - x


--b.
type D = Stack -> Stack
semCmd :: Cmd -> D
semCmd (LD x) s = x:s
semCmd ADD (x:y:s) = (x+y):s
semCmd MULT (x:y:s) = (x*y):s
semCmd DUP (x:s) = x:x:s
semCmd INC (x:s) = (x+1):s
semCmd SWAP (x:y:s) = y:x:s 
semCmd (POP i) s    = drop i s
--semCmd _ _ = Nothing

semStatTC :: Prog -> Maybe Stack
semStatTC p | rankP p /= Nothing = Just $ sem p []
            | otherwise = Nothing

sem :: Prog -> D
sem [] s = s
sem (v:p) s = sem p $ semCmd v s

{-The new type of sem is Prog -> Stack -> StackWe are simplifying it by removing the error domain.This can be done because we created a type checker tofirst make sure that sem only evaluates valid programs,so we will never pass a non valid program to sem.-}


--Exercise 2

data Shape = X
           | TD Shape Shape
           | LR Shape Shape
           deriving Show

type BBox = (Int,Int)

--a.

bbox :: Shape -> BBox
bbox X = (1,1)
bbox (TD a b) = (width, height)
               where (x1, y1) = bbox a
                     (x2, y2) = bbox b
                     width    = max x1 x2
                     height   = y1 + y2
bbox (LR a b) = (width, height)
               where (x1, y1) = bbox a
                     (x2, y2) = bbox b
                     width    = x1 + x2
                     height   = max y1 y2

--b.
rect :: Shape -> Maybe BBox
rect X = Just (1,1)
rect (TD a b)
    | x1 == x2 = Just $ bbox (TD a b)
    | otherwise = Nothing
    where  x1 = fmap fst $ rect a
           x2 = fmap fst $ rect b 
rect (LR a b)
    | y1 == y2 = Just $ bbox (LR a b)
    | otherwise = Nothing
    where y1 = fmap snd $ rect a
          y2 = fmap snd $ rect b 


