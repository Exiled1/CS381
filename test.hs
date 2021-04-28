(+++) :: (Num a) => Maybe a -> Maybe a -> Maybe a
(+++) Nothing Nothing = Nothing
(+++) (Just a) Nothing = Nothing
(+++) Nothing (Just a) = Nothing
(+++) (Just a) (Just b) = Just $ a + b

add :: Num a => a -> a -> a
add a b = a + b

chgLast :: Int -> [Int] -> [Int]
chgLast y [x] = [y]
chgLast y (x:xs) = x:chgLast y xs

min:: Maybe Integer -> Maybe Integer -> Maybe Integer
min Nothing Nothing = Nothing
min n Nothing  = n
min Nothing n  = n
min m n  = if m < n then m else n

plus :: (Num a) => Maybe a -> Maybe a -> Maybe a
plus Nothing y = Nothing
plus x Nothing = Nothing
plus m n = m +++ n

sum :: [Int] -> Int
sum xs = foldr (+) 0 xs

num1 = Just 5
num2 = Just 3

-- minChange :: (Num a) => a -> [a] -> Maybe(a)
-- minChange 0 ds = Just(0)
-- minChange a [] = Nothing
-- minChange a (d:ds) =
--     if d > a then
--         minChange(a, ds)
--     else
--         z = minChange(a - d, d:ds)
--         x = plus(Just(1), z)
--         y = minChange(a, ds)



        -- min (plus(Just(1), minChange(a-d, d:ds)), minChange(a, ds))


