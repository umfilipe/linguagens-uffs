comprimentoLista :: [Int] -> Int
comprimentoLista lista
  | null lista = 0
  | otherwise = 1 + comprimentoLista (tail lista)

somatorioLista :: [Int] -> Int
somatorioLista lista
  | null lista = 0
  | otherwise = head lista + somatorioLista (tail lista)

possuiChar :: [Char] -> Char -> Bool
possuiChar texto ch
  | null texto = False
  | head texto == ch = True
  | otherwise = possuiChar (tail texto) ch

maiorElemento :: [Int] -> Int
maiorElemento [] = -1
maiorElemento lista = if head lista >= maiorCauda then head lista else maiorCauda
  where
    maiorCauda = maiorElemento (tail lista)

raizes :: Float -> Float -> Float -> [Float]
raizes a b c
  | delta < 0 = []
  | delta == 0 = [(-b) / (2 * a)]
  | delta > 0 = [(-b - sqrt delta) / (2 * a), (-b + sqrt delta) / (2 * a)]
  where
    delta = b * b - 4 * a * c

tabuada :: Int -> [Int]
tabuada n = [n * x | x <- [1 .. 10]]

numeroPrimo :: Int -> Bool
numeroPrimo n = if length [x | x <- [1 .. n], mod n x == 0] == 2 then True else False
