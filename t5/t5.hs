-- 1) Escreva uma função addSuffix :: String -> [String] -> [String] usando list comprehension, para adicionar um dado sufixo às strings contidas numa lista.
addSuffix :: String -> [String] -> [String]
addSuffix a str = [x ++ a | x <- str ]

-- 2) Escreva uma função countShorts :: [String] -> Int, que receba uma lista de palavras e retorne a quantidade de palavras dessa lista que possuem menos de 5 caracteres. Use recursão.
countShorts :: [String] -> Int
countShorts [] = 0
countShorts (x:xs) 
  | length(x) < 5 = 1 + countShorts xs
  | otherwise = 0 + countShorts xs

-- 3) Reescreva a função do exercício acima, desta vez usando list comprehension.
countShortsLC :: [String] -> Int
countShortsLC list = length [x | x<-list, (length x)<5]

-- 4) Escreva uma função ciclo :: Int -> [Int] -> [Int] que receba um número N e uma lista de inteiros, retornando uma nova lista com N repetições da lista original.
ciclo :: Int -> [Int] -> [Int]
ciclo 0 _ = []
ciclo x list = list ++ ciclo (x-1) list

-- 5) Escreva uma função numera :: [String] -> [(Int,String)], que receba uma lista de palavras e retorne outra lista contendo tuplas com as palavras numeradas a partir de 1. Use recursão.
numera :: [String] -> [(Int,String)]
numera = aux 1

aux :: Int -> [String] -> [(Int, String)]
aux _[] = []
aux x list = (x, head list) : aux (x + 1) (tail list)

-- 6) Explique, em forma de comentário, o resultado de cada expressão abaixo.
-- [ (x,y) | x <- [1..5], even x, y <- [(x + 1)..6], odd y]
   -- ->[(2,3),(2,5),(4,5)]
-- Retorna uma lista de tuplas criada a partir dos valores pares de 1 a 5 de uma lista x e os valores ímpares entre x+1 e 6 para uma lista y, no qual retorna os 3 pares 3 pares (2,3), (2,5) e (4,5)


-- [ a ++ b | a <- ["lazy", "big"], b <- ["frog", "dog"]]
   -- ->["lazyfrog","lazydog","bigfrog","bigdog"]
-- Concatena todos os elementos da primeira lista com os da segunda lista

-- concat [ [a,'-'] | a <- "paralelepipedo", not (elem a "aeiou")]
   -- ->"p-r-l-l-p-p-d-"
-- Substitui as vogais da palavra paralelepipedo por "-", retornando uma string "p-r-l-l-p-p-d-"

-- 8) Nesta questão você deverá usar list comprehension. Suponha que um retângulo seja representado por uma tupla (Float,Float,Float,Float), contendo respectivamente as coordenadas x e y do ponto no seu canto superior esquerdo, seguidas das suas medidas de largura e altura. Sabendo que o eixo x cresce de cima para baixo e o eixo y da esquerda para direita, crie uma função genRects :: Int -> (Int,Int) -> [(Float,Float,Float,Float)] que receba um número N e um ponto (x,y) e gere uma sequência de N retângulos não sobrepostos. Os retângulos devem ser alinhados pelos seus topos, a partir do ponto dado, com largura e altura constantes. Por exemplo, usando largura e altura iguais a 5.5:
genRects :: Int -> (Int,Int) -> [(Float,Float,Float,Float)]
genRects n (x, y) = [((fromIntegral x)+xs, fromIntegral y, 5.5, 5.5) | xs <- [0.0, 5.5..5.5*(fromIntegral n-1)]]

-- 9) Escreva uma função recursiva que receba uma lista de tuplas e decomponha cada uma delas, gerando uma tupla de listas.
func1 :: [(Int,Int)] -> ([Int],[Int])
func1 [] = ([],[])
func1 (x:xs) = (fst x:(fst (func1 xs)), snd x: (snd (func1 xs)))

-- 10) Refaça o exercício anterior usando list comprehension.
func2 :: [(Int,Int)] -> ([Int],[Int])
func2 list = ([fst x|x<-list],[snd x|x<-list])

-- 11) Refaça o exercício anterior usando uma função de alta ordem.
func3 :: [(Int,Int)] -> ([Int],[Int])
func3 list = (map (\x-> fst(x)) list, map (\x-> snd(x)) list)