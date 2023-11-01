--Enum
data PuntoCardinale = Nord | Sud | Ovest | Est
    deriving Show

sinistra :: PuntoCardinale -> PuntoCardinale
sinistra Nord = Ovest
sinistra Ovest = Sud
sinistra Sud = Est
sinistra Est = Nord

destra :: PuntoCardinale -> PuntoCardinale
destra = sinistra . sinistra . sinistra 

indietro :: PuntoCardinale -> PuntoCardinale
indietro = sinistra . sinistra


--Constructor
data ForseInt = Niente | Proprio Int
  deriving Show

testa :: [Int] -> ForseInt
testa [] = Niente
testa (x : xs) = Proprio x

data Numero = F Float | I Int
    deriving Show


somma :: Numero -> Numero -> Numero
somma (I a) (I b) = I (a + b)
somma (F a) (I b) = F (a + fromIntegral b)
somma (I a) (F b) = F (fromIntegral a + b)
somma (F a) (F b) = F (a + b)

sommatoria :: [Numero] -> Numero
sommatoria = foldr somma (I 0)

proprio :: [ForseInt] -> [Int]
proprio [] = []
proprio (Niente : xs) = proprio xs
proprio (Proprio a : xs) = a : proprio xs

--Alberi
data Tree a = Leaf | Branch a (Tree a) (Tree a)
  deriving Show

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Branch x Leaf Leaf
insert x t@(Branch y t₁ t₂) | x == y    = t
                            | x < y     = Branch y (insert x t₁) t₂
                            | otherwise = Branch y t₁ (insert x t₂)

empty :: Tree a -> Bool
empty Leaf = True
empty _    = False


tmax :: Tree a -> a
tmax (Branch x _ Leaf) = x
tmax (Branch _ _ t)    = tmax t

tmin :: Tree a -> a
tmin (Branch x Leaf _) = x
tmin (Branch _ t _)    = tmin t

tminF :: Tree a -> Maybe a
tminF Leaf = Nothing
tminF (Branch x Leaf _) = Just x
tminF (Branch _ t _)    = tminF t

delete :: Ord a => a -> Tree a -> Tree a
delete _ Leaf = Leaf
delete x (Branch y t₁ t₂) | x < y = Branch y (delete x t₁) t₂
                          | x > y = Branch y t₁ (delete x t₂)
delete x (Branch _ t Leaf) = t
delete x (Branch _ Leaf t) = t
delete x (Branch _ t₁ t₂)  = Branch y (delete y t₁) t₂
  where
    y = tmax t₁


elements :: Tree a -> [a]
elements Leaf             = []
elements (Branch x t₁ t₂) = elements t₁ ++ [x] ++ elements t₂


treeSort :: Ord a => [a] -> [a]
treeSort xs = elements (foldr insert Leaf xs)

bst :: Ord a => Tree a -> Bool
bst Leaf = True
bst (Branch x t1 t2) = bst t1 && bst t2 
              && (empty t1 || tmax t1 < x)
              && (empty t2 || tmin t2 > x) 