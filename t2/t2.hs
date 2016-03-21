-- 1) Crie uma função somaQuad :: Int -> Int -> Int que calcule a soma dos quadrados de dois números x e y.
somaQuad :: Int -> Int -> Int
somaQuad x y = x^2 + y^2

-- 2) Crie uma função hasEqHeads :: [Int] -> [Int] -> Bool que verifique se 2 listas possuem o mesmo primeiro elemento. Use o operador lógico '==' para verificar igualdade.
hasEqHeads :: [Int] -> [Int] -> Bool
hasEqHeads x y = if (head x) == (head y) then True else False

-- 3) Escreva uma função que receba uma lista de nomes e adicione a string "Sr. " no início de cada nome
add :: [String] -> [String]
add list = map ("Sr. " ++)list

-- 4) Crie uma função que receba uma string e retorne o número de espaços nela contidos. Dica: aplique 2 funções consecutivamente.
cont :: String -> Int
cont list = length(list) - length(filter(/=' ')list)

-- 5) Escreva uma função que, dada uma lista de números, calcule 3*n^2 + 2/n + 1 para cada número n da lista. Dica: defina uma função anônima
calc :: [Int] -> [Int]
calc list = map(\n -> 3*n^2 + div 2 n + 1) list

-- 6) Escreva uma função que, dada uma lista de números, selecione somente os que forem negativos.
neg :: [Int] -> [Int]
neg x = filter(< 0)x

-- 7) Escreva uma função que receba uma lista de números e retorne somente os que estiverem entre 1 e 100, inclusive. Dica 1: use uma função anônima. Dica 2: use o operador '&&' para expressar um 'E' lógico.
sel :: [Int] -> [Int]
sel list = filter(\n -> n >= 1 && n <= 100)list

-- 8) Escreva uma função que, dada uma lista de idades de pessoas no ano atual, retorne uma lista somente com as idades de quem nasceu depois de 1970. Para testar a condição, sua função deverá subtrair a idade do ano atual.
idade :: [Int] -> [Int]
idade list = filter(\n -> n < (2016 - 1970))list

-- 9) Escreva uma função que receba uma lista de números e retorne somente aqueles que forem pares.
par :: [Int] -> [Int]
par list = filter(\n ->(mod n 2)==0)list

-- 10) Crie uma função charFound :: Char -> String -> Bool que verifique se o caracter (primeiro argumento) está contido na string (segundo argumento).
charFound :: Char -> String -> Bool
charFound _ "" = False
charFound x y = if x == (head y) then True else charFound x (tail y)

-- 11) A função takeWhile :: (a -> Bool) -> [a] -> [a] é uma função de alta ordem. Ela recebe uma função condicional e uma lista, retornando o "menor prefixo" (isto é, porção inicial) da lista que satisfaça a condição dada.
-- takeWhile :: (a -> Bool) -> [a] -> [a]
-- retorna uma lista com os elementos que satisfazem com a condicao fornecida
-- exemplos:
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2] -> n° de elemento menor que 3
-- takeWhile (< 9) [1,2,3] == [1,2,3] -> n° de elemento menor que 9
-- takeWhile (< 0) [1,2,3] == [] -> n° de elemento menor que 0

-- 12)Crie uma função que receba uma lista de nomes e retorne outra lista com somente aqueles nomes que terminarem com a letra 'a'
ult :: [String]-> [String]
ult list = filter (\x -> ('a' `elem` x)) list
