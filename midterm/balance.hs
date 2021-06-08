-- type Balance = Int

-- data Action = Deposit Int
--             | Withdraw Int
--             | Fee
--             | Sequ Action Action
type Account = Int

data Activity = Deposit Int
              | Withdraw Int
              | Close
              | After Activity Activity

-- sem :: Action -> Balance -> Balance
-- sem (Deposit a) bal = a + bal
-- sem (Withdraw w) bal
--     | bal - w < 0 = 0 -- Balance
--     | otherwise = bal - w
-- sem Fee bal = bal - 1
-- sem (Sequ a1 a2) bal = sem a2 (sem a1 bal)

-- ---- 

-- type Stack = [Int]
-- data Cmd = LD Int | (:...) | Recover | IfZ Cmd Cmd
-- semCmd :: Cmd -> Maybe Stack -> Maybe Stack
-- semCmd Recover (Just s) = Just $ s
-- semCmd Recover Nothing = Just $ []
-- semCmd (IfZ cm1 cm2) (Just (s:xs))
--     | s == 0 = semCmd cm1 (Just $ s:xs)
--     | otherwise = semCmd cm2 (Just $ s:xs)
type Path = Int
type D = (Path, Path)
type D1 = Either Path Path

sem :: Activity -> Account -> Account
sem (Deposit m) acc = m + acc
sem (Withdraw m) acc  | m > acc = 0
    | otherwise = acc - m
sem Close acc = 0
sem (After a1 a2) acc = sem a2 (sem a1 acc)