-- Ex 1
-- (a)
import Data.List hiding (group)

{-
Group 38Fun:
- Rodolfo Peralta
- Mitchell Radford
- Tu Lam
- Hugh MacWilliams
-}

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


------------------------------------------------------
-- Regex!!

data RegEx = Empty  -- Match empty shit
    | Dot   -- Match anything (.)
    | Ch Char   -- Matches itself.
    | QMark RegEx   -- Reg? Matches either nothing or the regex.
    | Star RegEx -- Match 0 or more occurences
    | Plus RegEx    -- Matches 1 or more occurences.
    | Seq RegEx RegEx -- Regex1 and Regex2
    | Or RegEx RegEx -- Reg1 | Reg2 matches either of them
    deriving Show

-- data Strings  = String

group :: String -> RegEx
group ""  = Empty
group (ch:str) = Seq (Ch ch) (group str)

accept :: RegEx -> String -> Bool
accept Empty str  = str == "" -- Returns true if it's empty. 
accept Dot str = str /= []
accept (Ch ch) str = [ch] == str
accept (QMark reg) str = accept (Or Empty reg) str
accept (Star reg) str = accept (Or (Plus reg) Empty) str
accept (Plus reg) str = accept (Seq reg (Star reg)) str
accept (Seq reg1 reg2) str = or [accept reg1 str1 && accept reg2 str2 | (str1, str2) <- splits str]
accept (Or reg1 reg2) str = accept reg1 str || accept reg2 str

commaSep :: RegEx
commaSep = Seq (Or (group "cat") (group "bat")) (Star(Seq (Ch ',') (Seq (Or (Ch 'c' ) (Ch 'b')) (group "at"))))








splits :: [a] -> [([a],[a])]
splits [] = []
splits [x] = [([],[x]),([x],[])]
splits (x:xs) = ([],x:xs) : [(x:s,t) | (s,t) <- splits xs]



classify :: RegEx -> [String] -> IO ()
classify e ws = putStrLn ("ACCEPT:\n"++show acc++"\nREJECT:\n"++show rej)
    where
        acc = filter (accept e) ws
        rej = filter (not.accept e) ws



commaSepTest :: [String]

commaSepTest = ["cat","cat,bat","cat,cat","bat","",",","dog",",cat","cat,","catcat","cat,,bat","cat,bat,", "cat,cat,cat", "cat,bat,cat", "bat,bat,cat", "cat,cat,bat,"]