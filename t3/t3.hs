-- 1) Defina uma função recursiva que receba uma lista de números inteiros e produza uma nova lista com cada número elevado ao quadrado.
eleva2 :: [Int] -> [Int]
eleva2 [] = []
eleva2 n = (head n)^2 : eleva2(tail n)

-- 2) Escreva uma função recursiva que receba uma lista de nomes e adicione a string "Sr. " no início de cada nome.
add :: [String] -> [String]
add [] = []
add list = ("Sr. " ++ (head list) ) : add (tail list)

-- 3) Crie uma função recursiva que receba uma string e retorne o número de espaços nela contidos.
cont :: String -> Int
cont [] = 0
cont list 
  | (head list) == ' ' = 1 + cont(tail list)
  | otherwise = 0 + cont(tail list)

-- 4) Escreva uma função recursiva que, dada uma lista de números, calcule 3*n^2 + 2/n + 1 para cada número n da lista.
calc :: [Float] -> [Float]
calc [] = []
calc x = (3*(head x)^2 +  2/(head x) + 1) : calc(tail x)

-- 5) Escreva uma função recursiva que, dada uma lista de números, selecione somente os que forem negativos.
neg :: [Int] -> [Int]
neg [] = []
neg list 
  | (head list) < 0 = (head list): neg(tail list)
  | otherwise = neg(tail list)

-- 6) Defina uma função não-recursiva que receba uma string e retire suas vogais, conforme os exemplos abaixo
semVogais :: String -> String
semVogais list = filter (\x -> not( x `elem` "aeiouAEIOU") ) list

-- 7) Expresse uma solução recursiva para o exercício anterior.
semVogaisrec :: String ->String
semVogaisrec "" = ""
semVogaisrec list
  |not( (head list) `elem` "aeiouAEIOU") = (head list) : semVogais (tail list)
  |otherwise = semVogais (tail list)

-- 8) Defina uma função não-recursiva que receba uma string, possivelmente contendo espaços, e que retorne outra string substituindo os demais caracteres por '-', mas mantendo os espaços.
codifica :: String -> String
codifica list = map (\x-> if x == ' ' then ' ' else '-') list

-- 9) Defina uma função recursiva que resolva o mesmo problema do exercício anterior
codificaRec :: String -> String
codificaRec "" = ""
codificaRec list
  |(head list) == ' ' = (head list) : codificaRec (tail list)
  | otherwise =  '-' : codificaRec (tail list)

-- 10) Crie uma função recursiva charFound :: Char -> String -> Bool, que verifique se o caracter (primeiro argumento) está contido na string (segundo argumento).
charFound :: Char -> String -> Bool
charFound _ "" = False
charFound x y
  | x == (head y) = True
  | otherwise = charFound x (tail y)

-- 11) Defina uma função recursiva que receba uma lista de coordenadas de pontos 2D e desloque esses pontos em 2 unidades
translate :: [(Float, Float)] -> [(Float, Float)]
translate [] = []
translate ((x,y):xs) = (x + 2, y + 2) : translate xs

-- 12) Defina uma função recursiva que receba 2 listas e retorne uma lista contendo o produto, par a par, dos elementos das listas de entrada
prodVet :: [Int]-> [Int] -> [Int]
prodVet _ [] = []
prodVet [] _ = []
prodVet (x:xs) (n:ns) = (x*n) : prodVet xs ns

-- 13) Resolva o exercício anterior usando uma função de alta ordem, eliminando a necessidade de escrever código com recursão.
prodVet1 :: [Int] -> [Int] -> [Int]
prodVet1 x n = zipWith (*) x n

-- 14) Defina uma função recursiva que receba um número n e retorne uma tabela de números de 1 a n e seus quadrados
geraTabela :: Int -> [(Int, Int)]
geraTabela 0 = []
geraTabela n = aux 1 n

aux :: Int -> Int -> [(Int, Int)]
aux _ 0 = []
aux 0 _ = []
aux i n
    | i <= n = (i, i ^ 2) : aux (i + 1) n
    | otherwise = []