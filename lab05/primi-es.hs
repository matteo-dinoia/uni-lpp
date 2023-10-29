primoAux :: Integer -> Integer -> Bool
primoAux k n    | k >= n             = k == n
                | n `mod` k == 0     = False
                | otherwise          = primoAux (k + 1) n

primo :: Integer -> Bool
primo = primoAux 2

primi :: Int -> [Integer]
primi n = take n (filter primo (enumFrom 2))


primoMaggioreDi :: Integer -> Integer
primoMaggioreDi n = head (filter primo (enumFrom (n + 1)))

primiGemelli :: Int -> [(Integer, Integer)] 
primiGemelli n = take n (filter (uncurry (\a -> \b -> b - a == 2)) (zip inf_primi (tail inf_primi)))
    where
        inf_primi = filter primo (enumFrom 2)

