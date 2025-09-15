-- função sem usar casamento de padrões
opp :: (Int, (Int, Int)) -> Int
opp z =
  if fst z == 1
    then fst (snd z) + snd (snd z)
    else
      if fst z == 2
        then fst (snd z) - snd (snd z)
        else 0

-- função simplificada usando casamento de padrões
opp2 :: (Int, (Int, Int)) -> Int
opp2 (1, (x, y)) = x + y
opp2 (2, (x, y)) = x - y
opp2 (_, (_, _)) = 0
