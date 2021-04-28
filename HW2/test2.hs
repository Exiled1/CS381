data Mode = Up | Down
            deriving Show

data Cmd = Pen Mode
        | MoveTo Int Int
        | Seq Cmd Cmd
           deriving Show

type State = (Mode, Int, Int)

type Line = (Int, Int, Int, Int)
type Lines = [Line]

semS :: Cmd -> State -> (State, Lines)
semS (Pen Up)        (_, x, y)    = ((Up, x, y), [])
semS (Pen Down)      (_, x, y)    = ((Down, x, y), [])
semS (MoveTo x y)    (Up, _, _)   = ((Up, x, y), [])
semS (MoveTo x' y')  (Down, x, y) = ((Down, x', y'), [(x, y, x', y')])
semS (Seq cmd0 cmd1) s0           = (s1, l0 ++ l1)
                                    where (s2, l1) = semS cmd1 s1
                                          (s1, l0) = semS cmd0 s0

sem' :: Cmd -> Lines
sem' cmd = snd (semS cmd (Up, 0, 0))