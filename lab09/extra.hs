import Data.Char (ord, chr)


union :: Ord a => [a] -> [a] -> [a]
union [] ys              = ys
union xs []              = xs
union (x : xs) (y : ys)  | x < y       = x : union xs (y : ys)
                         | x > y       = y : union (x : xs) ys
                         | otherwise   = x : union xs ys

intersection :: Ord a => [a] -> [a] -> [a]
intersection [] ys             = []
intersection xs []             = []
intersection (x : xs) (y : ys) | x > y        = intersection (x : xs) ys
                               | x < y        = intersection xs (y : ys)
                               | otherwise    = x : intersection xs ys

difference :: Ord a => [a] -> [a] -> [a]
difference [] _              = []
difference xs []             = xs
difference (x : xs) (y : ys) | x > y        = difference (x : xs) ys
                             | x < y        = x : difference xs (y : ys) 
                             | otherwise    = difference xs ys


getLines :: IO [String]
getLines = do   s <- getLine
                if null s then return []
                else do ss <- getLines
                        return (s : ss)

countChar :: [String] -> Int
countChar []       = 0
countChar (x : xs) = aux x + countChar xs
    where
        aux []       = 0
        aux (s : ss) = 1 + aux ss

countWord :: [String] -> Int
countWord []       = 0
countWord (x : xs) = aux2 x + countWord xs
    where
        aux2 :: String -> Int
        aux2 []       = 0
        aux2 (x : xs) | x == ' '    = aux2 xs
                      | otherwise   = 1 + aux (x : xs)
        aux :: String -> Int
        aux (a : b : c : ss) | (a /= ' ') && (b == ' ') &&  (c /= ' ')   = 1 + aux (c : ss)
                             | (a /= ' ') && (b == ' ')                  = aux (a : ' ' : ss)
                             | otherwise                                 = aux (b : c : ss)
        aux _                = 0

main :: IO ()
main = do   s <- getLines
            putStr "Numero di righe: "
            putStrLn (show (length s))
            putStr "Numero di caratteri: "
            putStrLn (show (countChar s))
            putStr "Numero di parole: "
            putStrLn (show (countWord s))

putStrLnMia :: String -> IO()
putStrLnMia = foldr (\c -> \ac -> putChar c >> ac) (putChar '\n')

putLinesMia :: [String] -> IO ()
putLinesMia = foldr (\s -> \ac -> putStrLnMia s >> ac) (return ())

pulisci :: String -> String
pulisci [] = []
pulisci (x : xs) | (ord x >= ord 'a' && ord x <= ord 'z')  = x : pulisci xs
                 | (ord x >= ord 'A' && ord x <= ord 'Z')  = chr (ord x + ord 'a' - ord 'A') : pulisci xs 
                 | otherwise                               = pulisci xs 

palindroma :: String -> Bool
palindroma xs = (reverse pul) == pul 
    where
        pul = pulisci xs

main2 :: IO ()
main2 = do  s <- getLines
            putLinesMia (filter palindroma s) 

------- ES ---------
pari :: [a] -> [a]
pari (a : b : xs) = a : pari xs 
pari xs = xs

dispari :: [a]-> [a]
dispari [] = []
dispari (x : xs) = pari xs
--------------------

type Name = String
data Term = Var Name | Lam Name Term | App Term Term

fv :: Term -> [Name]
fv (Var name) = [name]
fv (Lam name term) = difference (fv term) [name]
fv (App term1 term2) = union (fv term1) (fv term2)


occurs :: Name -> Term -> Bool
occurs x t = x `elem` fv t

normal :: Term -> Bool
normal (Lam x (App t (Var y))) | x == y && not (occurs x t) = False  -- WHY
normal (Var _) = True
normal (Lam _ term) = normal term
normal (App (Lam _ _) _) = False
normal (App term1 term2) = normal term1 && normal term2


data Tree a = Leaf | Branch a (Tree a) (Tree a)

binarySearchTree :: Ord a => Tree a -> Bool
binarySearchTree Leaf = True
binarySearchTree t@(Branch _ _ _) = fst (aux t)
    where
        aux :: Ord a => Tree a -> (Bool, (Int, Int))
        aux (Branch p Leaf Leaf)  = (True, (p, p))
        aux (Branch p t Leaf)     = aux t
        aux (Branch p Leaf t)     = aux t
        aux (Branch p tl tr)      = \a -> \b -> (fst a && fst b && p > (snd . snd a) && p < (fst . snd b)
                                                min (fst . snd a) (fst . snd b), 
                                                max (snd . snd a) (snd . snd b)) (aux tl) (aux tr)