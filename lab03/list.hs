media :: [Int] -> Float
media x = fromIntegral(sum x) / fromIntegral(length x)

--media2 :: [Int] -> Float
--media2 a : [b] =

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

