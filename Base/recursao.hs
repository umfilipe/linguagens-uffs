fatorial :: Int -> Int
fatorial n
  | n == 0 = 1
  | n > 0 = n * fatorial (n - 1)

modRecursivo :: Int -> Int -> Int
modRecursivo a b
  | b > a = a
  | a == b = 0
  | otherwise = modRecursivo (a - b) b

multiplicacaoRecursiva :: Int -> Int -> Int
multiplicacaoRecursiva a b
  | a == 1 = b
  | a == 0 = 0
  | b == 0 = 0
  | a > 1 = b + multiplicacaoRecursiva (a - 1) b

mdcRec :: Int -> Int -> Int
mdcRec x y
  | x == y = x
  | x > y = mdcRec (x - y) y
  | x < y = mdcRec y x
