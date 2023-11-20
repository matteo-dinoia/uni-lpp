import Data.Char (ord, chr)


showHex :: Int -> String
showHex x | x < 0      = '-' : showHex (negate x)
          | x == 0     = []
          | otherwise  = showHex (x `div` 16) ++ [hex (x `mod` 16)]  
    where 
        hex x   | x <= 9      = chr (x + ord '0')
                | x < 16      = chr ((x - 10) + ord 'A')



readHex :: String -> Int
readHex = aux 0
    where
        aux c [] = c
        aux c (x : xs) = aux (c * 16 + dehex x) xs
        dehex x | ord x <= ord '9' &&  ord x >= ord '0'    = ord x - ord '0'
                | ord x <= ord 'Z' &&  ord x >= ord 'A'    = 10 + ord x - ord 'A'
                | otherwise                                = 10 + ord x - ord 'a'
        
readHex2 :: String -> Int
readHex2 = foldl (\c -> \a -> c * 16 + dehex a) 0
    where
        dehex x | ord x <= ord '9' &&  ord x >= ord '0'    = ord x - ord '0'
                | ord x <= ord 'Z' &&  ord x >= ord 'A'    = 10 + ord x - ord 'A'
                | otherwise                                = 10 + ord x - ord 'a'

-- MATCHING DI RE
data RegExp a = Nil
              | Eps
              | Atom a
              | Sum (RegExp a) (RegExp a)
              | Seq (RegExp a) (RegExp a)
              | Star (RegExp a)
    deriving Show


nullable :: RegExp a -> Bool
nullable Nil       = False
nullable Eps       = True
nullable (Atom _)  = False
nullable (Sum f g) = nullable f || nullable g
nullable (Seq f g) = nullable f && nullable g
nullable (Star _)  = True



derive :: Eq a => a -> RegExp a -> RegExp a
derive a Nil        = Nil
derive a Eps        = Nil
derive a (Atom b)   | b == a      = Eps
                    | otherwise   = Nil
derive a (Sum x y)  = Sum (derive a x) (derive a y)
derive a (Seq x y)  | nullable x   = Sum (Seq (derive a x) y) (derive a y)
                    | otherwise    = Seq (derive a x) y
derive a (Star x)   = Seq (derive a x) (Star x)

match :: Eq a => RegExp a -> [a] -> Bool
match Nil _ = False
match re [] = nullable re
match re (x : xs) = match (derive x re) xs

empty :: RegExp a -> Bool 
empty Nil       = True
empty Eps       = False
empty (Atom _)  = False
empty (Sum f g) = empty f && empty g
empty (Seq f g) = empty f || empty g
empty (Star f)  = empty f

check :: RegExp a -> RegExp a
check (Seq Nil _)     = Nil
check (Seq _ Nil)     = Nil
check (Sum Nil a)     = a
check (Sum a Nil)     = a
check (Star Nil)      = Eps

normalize :: RegExp a -> RegExp a
normalize (Sum f g) = check (Sum (normalize f) (normalize g))
normalize (Seq f g) = check (Seq (normalize f) (normalize g))
normalize (Star f)  = check (Star (normalize f))
normalize a         = a

--Deriva simbolica

--Monadi
putStrLnMia :: String -> IO()
putStrLnMia = foldr (\c -> \ac -> putChar c >> ac) (putChar '\n')

putLinesMia :: [String] -> IO ()
putLinesMia = foldr (\s -> \ac -> putStrLnMia s >> ac) (return ())

putLinesBack :: [String] -> IO ()
putLinesBack = foldl (\ac -> \s -> putStrLnMia s >> ac) (return ())



getLines :: IO [String]
getLines = getLine >>= \s ->
            if null s then return []
            else getLines >>= \ss -> return (s : ss)

getInt :: IO Int
getInt = getLine >>= return . read 

somma :: IO ()
somma = getInt >>= \i -> sum i 0 >>= \res -> print res
    where
       sum 0 c = return c
       sum i c = getInt >>= \num -> sum (i - 1) (c + num)

-- NOTAZIONE DO
for :: [a] -> (a -> IO()) -> IO()
for xs f = foldr ((>>).f) (putChar '\n') xs

putStrLn3 :: String -> IO()
putStrLn3 s = for s (putChar) 

getLines3 :: IO [String]
getLines3 = do  s <- getLine
                if null s then return []
                else do ss <- getLines3
                        return (s : ss)
