dobro :: Int -> Int
dobro n = n + n

quadrado :: Int -> Int
quadrado n = n * n

mapInt :: (Int -> Int) -> [Int] -> [Int]
mapInt _ [] = []
mapInt f (prim : outros) = f prim : mapInt f outros

pares :: Int -> Bool
pares x = mod x 2 == 0

impares :: Int -> Bool
impares x = mod x 2 == 1

filtro :: (Int -> Bool) -> [Int] -> [Int]
filtro f [] = []
filtro f (cab : cauda)
  | (f cab) == True = cab : (filtro f cauda)
  | otherwise = filtro f cauda

maior :: Int -> Int -> Bool
maior x y = x > y

menor :: Int -> Int -> Bool
menor x y = x < y

buscaLista :: (Int -> Int -> Bool) -> [Int] -> Int
buscaLista _ [] = -1
buscaLista _ (cab : []) = cab
buscaLista f (cab : cauda) = if (f cab x) then cab else x
  where
    x = buscaLista f cauda
