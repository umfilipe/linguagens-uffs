-- where e let

areaHeron :: Float -> Float -> Float -> Float
areaHeron a b c = sqrt (s*(s-a)*(s-b)*(s-c))
    where
        s = (a+b+c)/2

equacaoQuadratica :: Float -> Float -> Float -> String
equacaoQuadratica a b c
    | delta > 0 = "2 raizes reais"
    | delta == 0 = "1 raiz real"
    | otherwise = "nenhuma raiz real"

    where
        delta = b*b - 4*a*c

areaHeronLet :: Float -> Float -> Float -> Float
areaHeronLet a b c = let s = (a+b+c)/2 in sqrt (s*(s-a)*(s-b)*(s-c))
