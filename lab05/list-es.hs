inverti2 :: [Int] -> [Int]
inverti2 xs = accum xs []
    where
        accum [] acc = acc
        accum (x:xs) acc = accum xs (x:acc)


--ES extra liste
last_sum :: [Int] -> Bool
last_sum = aux 0
    where
        aux n [x] = x == n
        aux n (x:xs) = aux (n + x) xs

last_sum_lib :: [Int] -> Bool
last_sum_lib xs = sum (tail ys) == head xs
    where ys = reverse xs

max_lists_bad :: [[a]] -> [[a]]
max_lists_bad xs = foldr aux [] xs 
    where
        aux a cs    | length cs == 0                 = [a]
                    | length a > length (head cs)    = [a]
                    | length a == length (head cs)   = a : cs
                    | otherwise                      = cs

max_lists :: Ord a => [[a]] -> [[a]]
max_lists xs = filter ((max ==) . length) xs
    where
        max = length(maximum xs)


mapr :: (a -> b) -> [a] -> [b]
mapr f xs = foldr (\a -> \c -> (f a) : c) [] xs

filterr :: (a -> Bool) -> [a] -> [a]
filterr f xs = foldr aux [] xs
    where 
        aux a cs    | f a            = a : cs
                    | otherwise      = cs


isSub :: Eq a => [a] -> [a] -> Bool
isSub [] _ = True
isSub _ [] = False
isSub (x : xs) (y : ys)     | x == y      = isSub xs ys
                            | otherwise   = isSub (x: xs) ys


allSub :: [a] -> [[a]]
allSub [] = [[]]
allSub (x : xs) = xss ++ map (x :) xss
  where
    xss = allSub xs
