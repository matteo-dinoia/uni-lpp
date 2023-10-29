media :: [Int] -> Float
media x = fromIntegral(sum x) / fromIntegral(length x)

{-
--media2 :: [Int] -> Float
--media2 a : [b] =
-}

intervallo :: Int -> Int -> [Int]
intervallo a b | a > b            = []
               | otherwise         =  a : intervallo (a + 1) b

primoAux :: Int -> Int -> Bool
primoAux k n    | k >= n             = k == n
                | n `mod` k == 0     = False
                | otherwise          = primoAux (k + 1) n

primo :: Int -> Bool
primo = primoAux 2

primi :: Int -> [Int]
primi = primiAux 2
    where
        primiAux a b | a <= b && primo a         = a : primiAux (a + 1) b
                     | a <= b                    = primiAux (a + 1) b
                     | otherwise                 = []

prodotto:: [Int] -> Int
prodotto []       = 1
prodotto (x : xs) = x * prodotto xs

inverti :: [Int] -> [Int]
inverti []       = []
inverti (x : xs) = (inverti xs) ++ [x]

sommaCongiunta :: [Int] -> [Int] -> [Int]
sommaCongiunta [] x = x
sommaCongiunta x [] = x
sommaCongiunta (x : xs) (y : ys)  = (x + y) : sommaCongiunta xs ys

fattorialeList :: Int -> Int
fattorialeList x = product [2..x]

insert :: Int->[Int]->[Int] --dato elemento e lista produce altra lista
insert x [] = [x]                -- fa insert ordinato
insert x (y : ys) | x <= y    = x : y : ys
                  | otherwise = y : insert x ys

insertion_sort :: [Int] -> [Int]
insertion_sort [] = []
insertion_sort (x : xs) = insert x (insertion_sort xs)


merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | y <= x         = y : merge (x:xs) ys
                    | otherwise      = x : merge xs (y:ys)

split :: [Int] -> ([Int], [Int])
split [] = ([],[])
split [x] = ([x], [])
split (x:y:zs) = (x:xs, y:ys)
    where
        (xs, ys) = split zs

merge_sort :: [Int] -> [Int]
merge_sort [] = []
merge_sort [x] = [x]
merge_sort [x, y] | x <= y        = [x,y]
                  | otherwise     = [y,x]
merge_sort xs = merge (merge_sort ys) (merge_sort zs)
    where
        (ys, zs) = split2 xs

split2 :: [Int] -> ([Int], [Int])
split2 xs = (take n xs, drop n xs)
    where
        n = length xs `div` 2



