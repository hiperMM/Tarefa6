-- Aluno Matheus Monteiro
-- Tarefa 6 | Programacao funcional

----------------------------------------------------------------
{-
1. Usando List Comprehension escreva uma função, chamada divisoresden, que devolva uma
lista dos divisores de um número dado.
-}

divisoresden :: Int -> [Int]
divisoresden n = [x | x <- [1 .. n], n `mod` x == 0]

----------------------------------------------------------------
{-
Usando  List Comprehension  escreva  uma  função,  chamada  contaCaractere,  que  conte  a
ocorrência de um caractere específico, em uma string dada.
-}

contaCaractere :: String -> Char -> Int
contaCaractere palavra caractere = length [x | x <- palavra, x == caractere]

----------------------------------------------------------------
{-
Usando List Comprehension escreva uma função, chamada dobroNaoNegativo, que devolve
o dobro dos valores dos elementos não negativos da lista de inteiros dada.
-}

dobroNaoNegativo :: [Int] -> [Int]
dobroNaoNegativo lista = [x * 2 | x <- lista, x >= 0]

----------------------------------------------------------------
{-
Usando List Comprehension escreva uma função, chamada pitagoras, que devolva uma lista
de triplas, não repetidas, contendo os lados dos triângulos retângulos possíveis de serem
construídos por inteiros entre 1 e um número inteiro dado.
-}

pitagoras :: Int -> [(Int, Int, Int)]
pitagoras numero =
  [ (x, y, z) | x <- [1 .. numero], y <- [1 .. numero], z <- [1 .. numero], x ^ 2 + y ^ 2 == z ^ 2
  ]

----------------------------------------------------------------
{-
Números  perfeitos  são  aqueles  cuja  soma  dos  seus  divisores  é  igual  ao  próprio  número.
Usando List Comprehension escreva uma função, chamada numerosPerfeitos, que devolva
uma lista contendo todos os números perfeitos menores que um número dado. Lembre-se
que você já tem uma função que devolve uma lista dos divisores de um número dado.
-}

numeroPerfeito :: Int -> Bool
numeroPerfeito numero = numero == sum (init(divisoresden (numero)))

numerosPerfeitos :: Int -> [Int]
numerosPerfeitos numero = [x | x <- [1 .. numero], numeroPerfeito x]

----------------------------------------------------------------
{-
Usando List Comprehension escreva uma função, chamada produtoEscalar, que devolva o
produto escalar entre duas listas de inteiros. Lembre-se, existem as funções fst, snd e zip no
prelude que podem ser úteis.
-}

produtoEscalar :: [Int] -> [Int] -> Int
produtoEscalar lista1 lista2 = sum[fst x * snd x | x <- zip lista1 lista2]
----------------------------------------------------------------
{-
Usando  List Comprehension  escreva  uma  função,  chamada  primeirosPrimos,  que  devolva
uma lista contendo os n primeiros números primos a partir do número 2.
-}

numeroPrimo :: Int -> Bool
numeroPrimo numero = divisoresden numero == [1, numero]

primeirosPrimos :: Int -> [Int]
primeirosPrimos numero = [x | x <- [2 .. numero], numeroPrimo x]

----------------------------------------------------------------
{-
Usando  List Comprehension  escreva  uma  função,  chamada  paresOrdenados,  que  devolva
uma  lista  de  par  ordenados  contendo  uma  potência  de  2  e  uma  potência  de  3  até  um
determinado número dado. Observe que estes números podem ser bem grandes.
-}
----------------------------------------------------------------

paresOrdenados :: Int -> [(Int, Int)]
paresOrdenados numero = [(2^x, 3^x) | x <- [1 .. numero]]

----------------------------------------------------------------
main = do
  --------------------------------
  --questão 1
  print (divisoresden 10)
  --questão 2
  print (contaCaractere "caractere" 'r')
  --questão 3
  print (dobroNaoNegativo [2, -3, 4, 7, -8, 10, 91])
  --questão 4
  print (pitagoras 10)
  --questão 5
  print (numerosPerfeitos 10)
  --questão 6
  print(produtoEscalar [1,2,3] [4,5,6])
  --questão7
  print (primeirosPrimos 15)
  --questão8
  print (paresOrdenados 5)
