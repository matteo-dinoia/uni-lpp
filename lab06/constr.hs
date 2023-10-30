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