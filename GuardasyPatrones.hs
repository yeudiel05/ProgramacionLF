----GUARDAS Y PATRONES

--1
division :: Double -> Double -> Double
division _ 0 = 9999
division x y = x / y

--2
tabla :: Bool -> Bool -> Bool
tabla True False = True
tabla False True = True
tabla _ _ = False

--3
mayorRectangulo :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
mayorRectangulo (b1, h1) (b2, h2)
  | b1 * h1 >= b2 * h2 = (b1, h1)
  | otherwise = (b2, h2)

--4
intercambia :: (a, b) -> (b, a)
intercambia (x, y) = (y, x)

--5
raiz :: (Double, Double) -> (Double, Double) -> Double
raiz (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

--6
ciclo :: [a] -> [a]
ciclo xs
  | null xs   = []                  
  | otherwise = last xs : init xs     

--7
numeroMayor :: Integer -> Integer -> Integer
numeroMayor x y = max (read (show x ++ show y) :: Integer) (read (show y ++ show x) :: Integer)

--8
--raices a b c = b^2 - 4 * a * c
nraices :: (Ord t, Floating t) => t -> t -> t -> Int
nraices a b c
  | discriminante > 0 = 2
  | discriminante == 0 = 1
  | otherwise = 0
  where
    discriminante = b^2 - 4 * a * c

--9 raices a b c = b^2 - 4*a*c
raices :: Double -> Double -> Double -> [Double]
raices a b c
  | discriminante > 0 = [(-b + sqrt discriminante) / (2 * a), (-b - sqrt discriminante) / (2 * a)]
  | discriminante == 0 = [(-b) / (2 * a)]
  | otherwise = []
  where discriminante = b^2 - 4*a*c

--10 area a b c = (a + b + c) / 2
area :: Double -> Double -> Double -> Double
area a b c = sqrt (s * (s - a) * (s - b) * (s - c))
  where s = (a + b + c) / 2

--11 interseccion [a,b] [c,d] = [max a b, min c d ]
interseccion :: Ord a => [a] -> [a] -> [a]
interseccion [] _ = []
interseccion _ [] = []
interseccion [x1, x2] [y1, y2]
  | x2 < y1 || y2 < x1 = []
  | otherwise = [max x1 y1, min x2 y2]

--12
linea :: Integer -> [Integer]
linea n = [start..end]
  where
    start = n * (n - 1) `div` 2 + 1
    end = start + n - 1


---------------------------------------
------------RECURSIVIDAD----------------
--1
potencia :: Double -> Int -> Double
potencia _ 0 = 1
potencia x n = x * potencia x (n - 1)

--2
minimoComunDivisor :: Integer -> Integer -> Integer
minimoComunDivisor a 0 = a
minimoComunDivisor a b = minimoComunDivisor b (a `mod` b)

-- 3 Definir por recursión la función pertenece
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece x (y:ys) = (x == y) || pertenece x ys

--4 tomar elementos
-- Definir por recursión la función tomar
tomar :: Int -> [a] -> [a]
tomar 0 _ = []
tomar _ [] = []
tomar n (x:xs) = x : tomar (n - 1) xs

--5
-- Definir por comprensión la función digitosC
digitosC :: Integer -> [Integer]
digitosC n = [read [x] :: Integer | x <- show n]

--6
sumaDigitosR :: Integer -> Integer
sumaDigitosR 0 = 0
sumaDigitosR n = (n `mod` 10) + sumaDigitosR (n `div` 10)

-----2.1
ordenaRapida :: Ord a => [a] -> [a]
ordenaRapida [] = []  -- Caso base: lista vacía
ordenaRapida (x:xs) = ordenaRapida menores ++ [x] ++ ordenaRapida mayores
  where
    menores = [y | y <- xs, y <= x]
    mayores = [y | y <- xs, y > x]
