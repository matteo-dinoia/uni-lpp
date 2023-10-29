--Integrazione Numerica
occorrenze :: Eq a => a -> [a] -> Int
occorrenze x = length . filter (== x)

match :: Eq a => [a] -> [a] -> Bool
match xs ys = any (uncurry (==)) (zip xs ys)

adiacenti :: Eq a => [a] -> Bool
adiacenti xs = match xs (tail xs)

poli :: [Float] -> Float -> Float
--poli ls x = foldl (\c -> \a -> c * x + a) 0 ls 
poli cs x = sum (map (uncurry (*)) (zip cs (map (x ^) [0..])))

ordinata :: Ord a => [a] -> Bool -- ord. crescente
ordinata xs = all (uncurry (<=)) (zip xs (tail xs))

--perfetto :: Int -> Bool
--perfetto x = sum [d | d<-[1..x - 1], x `mod` d == 0] == x

trapezi :: (Double -> Double) -> Double -> Double -> Int -> Double
trapezi f a b n = sum . map (\x -> \y -> (x + y) / 2 * ((b - a) / n)) [(a + fromIntegral x * (b - a) / fromIntegral n, a + (fromIntegral x + 1) * (b - a) / fromIntegral n) | x <- [0..n - 1]]