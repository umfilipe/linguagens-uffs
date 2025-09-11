fatorial :: Int -> Int
fatorial n
    | n == 0 = 1
    | n > 0 = n * fatorial (n-1)

modRecursivo :: Int -> Int -> Int
modRecursivo a b
    | b > a = a
    | a == b = 0
    | otherwise = modRecursivo (a-b) b
