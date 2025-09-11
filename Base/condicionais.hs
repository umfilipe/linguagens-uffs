maior :: Int -> Int -> Int
maior a b = if a > b
    then a
    else b

maiorb :: Int -> Int -> Int
maiorb a b
    | a > b = a
    | a < b = b
    | otherwise = 0

par :: Int -> Bool
par a = if mod a 2 == 0 then True else False

charCase :: Char -> String
charCase ch
    | ch >= 'a' && ch <= 'z' = "minusculo"
    | ch >= 'A' && ch <= 'Z' = "maiusculo"
    | otherwise = "desconhecido"

parametrosTeste :: Int -> Int -> Int -> Int
parametrosTeste a b c
    | a == 0 = b*b + 3*c
    | a == 1 = 2*c*c - 3*c
    | a == 2 = 3*c - b*b
    | otherwise = 0
