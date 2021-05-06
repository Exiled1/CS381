module Hw3 where
import SVG

data Cmd = LD Int
    | ADD
    | MULT
    | DUP
    deriving Show

type Stack = [Int]
type Prog = [Cmd]

sem :: Prog -> Stack -> Maybe Stack
sem [] s = Just s
sem (x:xs) s = semCmd x s >>= sem xs


semCmd :: Cmd -> Stack -> Maybe Stack
semCmd (LD i) [] = Just [i]
semCmd (LD i) x = Just (i:x)
semCmd DUP (x:xs) = Just $ x:x:xs
semCmd _ [] = Nothing
semCmd _ [_] = Nothing
semCmd ADD (x:(y:xs)) = Just $ (x + y):xs
semCmd MULT (x:(y:xs)) = Just $ (x * y):xs
prog1 :: Prog
prog1 =  [DUP]

s1 :: Stack
s1 = []

-- PROG 2 -- 
data Cmd' = Pen Mode
    | MoveTo Int Int
    | Seq Cmd' Cmd'

data Mode = Up | Down
    deriving (Show)

type State = (Mode, Int, Int)

type Line = (Int, Int, Int, Int)

--type Lines = [Line]

semS :: Cmd' -> State -> (State, Lines)
semS (Pen Up) (_, x, y) = ((Up, x, y),[]) -- Pen up, regardless of mode, moves to X Y
semS (Pen Down) (_, x, y) = ((Down, x, y),[]) -- Same thing for here but pen's down.
-- semS (MoveTo x y) (m, x, y) = 
semS (Seq c1 c2) x = (state1, lines)
    where
        (state, lines1) = semS c1 x
        (state1, lines2) = semS c2 state
        lines = lines1 ++ lines2

-- totallyALine :: (Int, Int, Int, Int) -> Cmd'
-- totallyALine (x1, y1, x2, y2) = foldl Seq (Pen Up) [Pen Up,MoveTo x1 y1,Pen Down,MoveTo x2 y2,Pen Up]

-- lineStuff ()

sem' :: Cmd' -> Lines
sem' (Pen _) = []
sem' (MoveTo x y) = snd (semS (MoveTo x y) (Up, 0, 0))
-- sem' (Seq c1 c2) = 











