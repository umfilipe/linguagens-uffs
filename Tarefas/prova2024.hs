-- ex1
-- "find"

-- ex2
-- "abc"

-- ex3
-- nenhuma

-- ex4
-- [5,7,9]

-- ex5
-- nenhuma

-- ex6a

type Matricula = Integer

type Nome = String

type Notas = (Double, Double, Double)

type Estudante = (Matricula, Nome, Notas)

type Turma = [Estudante]

exemplo :: Turma
exemplo =
  [ (1, "Pedro", (5.0, 7.5, 4.5)),
    (2, "Maria", (9.0, 8.0, 10.0)),
    (3, "João", (3.0, 7.0, 4.5)),
    (4, "Paulo", (7.0, 5.0, 9.5)),
    (5, "Ana", (8.5, 8.0, 9.0))
  ]

exemplo2 :: Turma
exemplo2 =
  [ (2, "Maria", (9.0, 8.0, 10.0)),
    (3, "João", (3.0, 7.0, 4.5)),
    (4, "Paulo", (7.0, 5.0, 9.5)),
    (5, "Ana", (8.5, 8.0, 9.0))
  ]

mediaAluno :: Estudante -> Double
mediaAluno (_, _, (n1, n2, n3)) = (n1 + n2 + n3) / 3

mediaTurma :: Turma -> Double
mediaTurma turma = somaMedias turma / fromIntegral (contaAlunos turma)
  where
    somaMedias [] = 0
    somaMedias (x : xs) = mediaAluno x + somaMedias xs

    contaAlunos [] = 0
    contaAlunos (_ : xs) = 1 + contaAlunos xs

-- ex6b

aprovados :: Turma -> Turma
aprovados turma = filter (\aluno -> mediaAluno aluno >= 6.0) turma

-- ex7c

maiorMedia :: Turma -> Turma -> Turma
maiorMedia t1 t2 = if mediaTurma t1 > mediaTurma t2 then t1 else t2

-- ex7d

mediasTurma :: Turma -> [(String, Double)]
mediasTurma [] = []
mediasTurma ((_, nome, notas) : xs) =
  (nome, mediaAluno (0, nome, notas)) : mediasTurma xs
