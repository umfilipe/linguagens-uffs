-- ex1

salario :: Float -> Float
salario x = x + (x * 0.1) - (x * 0.07)

-- ex2

conceito :: Float -> Float -> Float -> Char
conceito x y z
  | m >= 8.0 = 'A'
  | m >= 7.0 = 'B'
  | m >= 6.0 = 'C'
  | m >= 5.0 && m < 6.0 = 'D'
  | otherwise = 'E'
  where
    m = (x * 0.2) + (y * 0.3) + (z * 0.5)

-- ex3

precoBase :: Integer -> Double
precoBase x
  | x == 1 = 100
  | x == 2 = 130
  | x == 3 = 150
  | x == 4 = 165
  | x == 5 = 175
  | x == 6 = 180
  | x >= 7 = 185

precoRetrato :: Integer -> String -> Double
precoRetrato pessoas dia
  | dia == "sabado" || dia == "domingo" = base * 1.2
  | otherwise = base
  where
    base = precoBase pessoas

-- ex4

fatorialDuplo :: Integer -> Integer
fatorialDuplo x
  | x <= 0 = 1
  | otherwise = x * fatorialDuplo (x - 2)

-- ex5

potenciaRecursiva :: Integer -> Integer -> Integer
potenciaRecursiva x n
  | n == 0 = 1
  | otherwise = x * potenciaRecursiva x (n - 1)

-- ex6

aumentos :: Float -> Integer -> Float
aumentos salario 0 = salario
aumentos salario n =
  aumentos (salario * (1 + taxa n)) (n - 1)

taxa :: Integer -> Float
taxa n = 0.015 * (2 ^^ (n - 1))

salarioAtual :: Float -> Integer -> Integer -> Float
salarioAtual salarioInicial anoEntrada anoAtual =
  aumentos salarioInicial anos
  where
    anos = fromIntegral (anoAtual - anoEntrada)
