-------------------------------------------------------------- Exercise 1
-------------------------------------------------------------- Mini Logo
-- cmd ::= pen mode
--      | moveto (pos, pos)
--      | def name (pars) cmd
--      | call name (vals)
--      | cmd; cmd
-- 
-- mode ::= up | down
-- pos ::= num | name
-- pars ::= name, pars | name
-- vals ::= num, vals | num

-- unspecifed nonterminals: [cmd, pos, name, pars, vals, mode, num]
-------------------------------------------------------------- Part a
-- define abstract syntax of mini logo as data type -- _con = constructor
data Cmd = Pen Mode
         | Moveto Pos Pos
         | Def String Pars Cmd
         | Call String Vals
         | Cmd_con [Cmd]

data Mode = Up | Down deriving(Show)
data Pos = Pos_int Int | Pos_str String

type Pars = [String]
type Vals = [Int]

-------------------------------------------------------------- Part b
-- Write mini logo macro vector to draw line from (x1, y1) to (x2, y2)
-- vector -- Def vector "x1", "y1", "x2", "y2" \n Moveto "x1" "y1" \n SetMode(Down) \n Moveto "x2" "y2"
vector :: Cmd 
commands = Cmd_con [
 Moveto (Pos_str "x1") (Pos_str "y1"),
 Pen Down, 
 Moveto (Pos_str "x2") (Pos_str "y2")
 ]
vector = Def "vector" 
    ["x1","y1","x2","y2"]
    (Cmd_con [
        Pen Up,
        Moveto (Pos_str "x1") (Pos_str "y1"),
        Pen Down,
        Moveto (Pos_str "x2")  (Pos_str "y1"),
        Pen Up
    ])

-------------------------------------------------------------- Part C 
-- Draws a stair of n steps
-- Prerequisites -- Moveto 0 0 \n SetMode(Down) \n Moveto 0 1 \n Moveto 1 1
line :: (Int,Int,Int,Int) -> Cmd
line (x1,y1,x2,y2) = 
    Cmd_con [
        Pen Up,
        Moveto (Pos_int x1) (Pos_int y2),
        Pen Down,
        Moveto (Pos_int x2)  (Pos_int y2),
        Pen Up
    ]
 

steps :: Int -> Cmd
steps 1 = Cmd_con [
    line (1,1,1,1)
    ] -- base case
steps n = Cmd_con [
  steps (n-1),
  line (n-1,n,n,n),
  line (n-1,n-1,n-1,n)
  ]

-------------------------------------------------------------- Helper
-- Pen Up -- SetMode(Up/Down)
-- Moveto (Pos_int 1) (Pos_int 2) -- Moveto 1 2
instance Show Cmd where
  show (Pen x) = "SetMode("++show x++")\n"
  show (Moveto x y) = "Moveto "++show x++" "++show y++"\n"
  show (Def x y z) = "Def "++x++" "++show y++"\n"++show z
  show (Cmd_con xs) = show xs

instance Show Pos where -- show values but without the cmd
  show (Pos_int x) = show x
  show (Pos_str x) = show x 