promedio :: Float -> Float -> Float -> Float
promedio x y z = (x + y + z) / x

sumaMonedas :: Int -> Int -> Int -> Int -> Int -> Int
sumaMonedas a b c d e = a + b*2 + c*5 + d*10 + e*20

volumenEsfera :: Float -> Float
volumenEsfera r = (4/3) * pi * r^3

areaCorona :: Float -> Float -> Float
areaCorona r1 r2 = pi * (r2^2 - r1^2)

ultimaCifra :: Integral a => a -> a
ultimaCifra x = x `rem` 10

