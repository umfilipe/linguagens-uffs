area :: Float -> Float
area r = pi*r*r

perimetro :: Float -> Float
perimetro r = 2*pi*r

hipotenusa :: Float -> Float -> Float
hipotenusa a b = sqrt (a*a + b*b)

diferencaArea :: Float -> Float -> Float
diferencaArea r1 r2 = abs (area r1 - area r2)
