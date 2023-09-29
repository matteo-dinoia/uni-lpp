assoluto :: Int -> Int
assoluto n | n >= 0    = n
           | True      = negate n

pari :: Int -> Bool
pari n = n `mod` 2 == 0

-- ES 1
esUno :: Int -> Int
esUno n = if pari n then n + 1 else assoluto n

esUnoB :: Int -> Int
esUnoB n | pari n         = n + 1
         | otherwise      = assoluto n

-- ES 2
bisestile :: Int -> Bool
bisestile n = n `mod` 400 == 0 || (n `mod` 4 == 0 && n `mod` 100 /= 0)


giorni :: Int -> Int
giorni n | bisestile n      = 366
         | otherwise        = 365

-- RIC
fact :: Int -> Int
fact n  | n == 0          = 1
        | otherwise       = n * fact (n - 1)

fattoriale :: Int -> Int
fattoriale 0 = 1
fattoriale n = n * fattoriale (n - 1)



fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

--ES 1
sums :: Int -> Int
sums 0 = 0
sums n = n + sums (n - 1)

--ES 2
exp2 :: Int -> Int
exp2 0 = 1
exp2 n = 2 * exp2 (n - 1)

--ES 3
bits :: Int -> Int
bits 0 = 0
bits n = n `mod` 2 + bits(n `div` 2)

--ES 4
isPow2 :: Int -> Bool
isPow2 1 = True
isPow2 n = if n `mod` 2 == 0 then isPow2(n `div` 2) else False

-- PIU' ARGOMENTI
--ES 1
massimo :: Int -> Int -> Int
massimo n m | n >= m         = n
            | otherwise     = m

minimo :: Int -> Int -> Int
minimo n m | n <= m         = n
           | otherwise     = m

--ES 2
potenza :: Int -> Int -> Int
potenza b 0 = 1
potenza b e = b * potenza b (e - 1)

--ES 3
pow2 :: Int -> Int
pow2 = potenza 2

--ES 4
sottrazione :: Int -> Int -> Int
sottrazione x y = x - y

sott0 :: Int -> Int
sott0 = sottrazione 0

--DA ITERAZIONE A RICORSIONE
--FIBO
fiboSup :: Int -> Int -> Int -> Int
fiboSup a b 0 = a
fiboSup a b v = fiboSup b (a + b) (v - 1)

fibo :: Int -> Int
fibo = fiboSup 0 1
