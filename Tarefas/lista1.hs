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

-- ex7

ultimo :: [Int] -> [Int]
ultimo [] = []
ultimo (cabeca : cauda) = if cauda /= [] then ultimo cauda else [cabeca]

-- ex8

primeiros :: [Int] -> [Int]
primeiros [_] = []
primeiros (cabeca : cauda) = cabeca : primeiros cauda

-- ex9

produtoLista :: [Int] -> [Int] -> [Int]
produtoLista [] _ = []
produtoLista _ [] = []
produtoLista (cabeca1 : cauda1) (cabeca2 : cauda2) = (cabeca1 * cabeca2) : produtoLista cauda1 cauda2

-- ex10 e ex11

data FormaComercializacao = Un | Peso
  deriving (Show)

data Produto
  = Perecivel Int String Int Bool FormaComercializacao
  | NaoPerecivel Int String String Int FormaComercializacao
  deriving (Show)

-- ex12

validade :: Produto -> Int -> Bool
validade (Perecivel _ _ anoValidade _ _) anoAtual = anoAtual <= anoValidade
validade _ _ = True

-- ex13

and_ :: Bool -> Bool -> Bool
and_ True True = True
and_ _ _ = False

or_ :: Bool -> Bool -> Bool
or_ False False = False
or_ _ _ = True

-- ex14

padroesLista :: [Int] -> Int
padroesLista [] = 0
padroesLista [x] = x
padroesLista (x : y) = x + head y

-- ex15

contador :: [Int] -> Int
contador l = foldl (\x y -> x + 1) 0 l

-- extra

contadorRecursivo :: [Int] -> Int
contadorRecursivo [cab] = 1
contadorRecursivo [] = 0
contadorRecursivo (_ : cauda) = 1 + contadorRecursivo cauda

-- ex16
-- a: "def"
-- b: 64
-- c: erro de tipo
-- d: [6,8]
-- e: 9
-- f: 60
