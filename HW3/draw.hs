{-
Group 38Fun:
- Rodolfo Peralta
- Mitchell Radford
- Tu Lam
- Hugh MacWilliams
-}
module Draw where

data Cmd = Pen Mode
         | MoveTo Pos Pos
         | Def String Pars Cmd
         | Call String Vals
         | Cmds [Cmd]


data Mode = Up | Down
    deriving Show

data Pos = I Int | S String


type Pars = [String]

type Vals = [Int]

instance Show Cmd where
  show (Pen x) = "SetMode("++show x++")\n"
  show (MoveTo x y) = "Moveto "++show x++" "++show y++"\n"
  show (Def x y z) = "Def "++x++" "++show y++"\n"++show z
  show (Cmds xs) = show xs

instance Show Pos where -- show values but without the cmd
  show (I x) = show x
  show (S x) = show x


-- (b)
-- Write a Mini Logo macro vector that draws a line from a given position (x1,y1) to a given position (x2,y2)

-- and represent the macro in abstract syntax, that is, as a Haskell data type value.



vector :: Cmd

vector = Def "Vector"
    ["x1", "y1", "x2", "y2"]
    (Cmds[
        Pen Up,
        MoveTo (S "x1") (S "y1"),
        Pen Down,
        MoveTo (S "x2") (S "y2"),
        Pen Up
    ])

-- (Ch)
-- Define a Haskell function steps :: Int -> Cmd that constructs a Mini Logo program which draws a stair of
-- n steps. Your solution should not use the macro vector.

totallyALine :: (Int, Int, Int, Int) -> Cmd
totallyALine (x1, y1, x2, y2) =
        Cmds[
            Pen Up,
            MoveTo (I x1) (I y1),
            Pen Down,
            MoveTo (I x2) (I y2),
            Pen Up
        ]


steps:: Int -> Cmd
steps 0 = totallyALine (0, 0, 0, 0)
steps 1 =
    Cmds [
        totallyALine (0, 0, 0, 0),
        totallyALine (0, 0, 0, 1),
        totallyALine (0, 1, 1, 1)
    ]


steps n =
    Cmds[
        steps (n-1),
        totallyALine (n-1, n-1, n-1, n),
        totallyALine (n-1, n, n, n)
    ]

