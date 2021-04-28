import Prelude hiding (min)

import Control.Applicative

addMaybe a b = a + b

(+++) :: (Num a) => Maybe(a) -> Maybe(a) -> Maybe(a)
(+++) Nothing Nothing = Nothing
(+++) (Just a) Nothing = Nothing
(+++) Nothing (Just a) = Nothing
(+++) (Just a) (Just b) = Just $ addMaybe a b


chgLast :: Int -> [Int] -> [Int]
chgLast y [x] = [y]
chgLast y (x:xs) = x:chgLast y xs

min:: Maybe(Integer) -> Maybe(Integer) -> Maybe(Integer)
min Nothing Nothing = Nothing
min n Nothing  = n
min Nothing n  = n
min m n  = if m < n then m else n

plus :: Maybe(Integer) -> Maybe(Integer) -> Maybe(Integer)
plus Nothing y = Nothing
plus x Nothing = Nothing
plus m n = m +++ n

num1 = Just(5)
num2 = Just(6)