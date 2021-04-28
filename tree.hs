member :: Int -> [Int] -> Bool
member x [] = False
--member x (y:ys) | x==y = true
--                | othwerwise = member x ys
member x (y:ys) = x==y || member x ys

insert :: Int -> [Int] -> [Int]
insert x [] = x:[]
insert x (y:ys)    | x <= y    = x:y:ys
                | otherwise = y:insert x ys

data Grade = A | B | C | D | F 
             deriving (Show,Eq,Ord)

data Tree = Node Int Tree Tree 
          | Leaf 
           deriving (Eq)

instance Show Tree where
    show Leaf = "_"
    show (Node x Leaf Leaf) = show x
    show (Node x l r) = show x++"{"++show l++", "++show r++"}"

l = Node 3 (Node 1 Leaf Leaf) (Node 5 Leaf Leaf)
t = Node 6 l (Node 9 Leaf (Node 8 Leaf Leaf))

find :: Int -> Tree -> Bool
find _ Leaf = False
find i (Node j l r) = i==j || find i l || find i r
--find i (Node J l r) | i==j = True
--                      | i<j  = find i l
--                      | True = find i r

minT :: Tree -> Int
minT (Node x Leaf _) = x
minT (Node _ t _)     = minT t
