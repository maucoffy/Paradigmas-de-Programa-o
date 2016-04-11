-- 12) O código em validaCPF.hs ilustra a validação dos dígitos verificadores de um CPF. Este código usa let para definir subexpressões, isto é, expressões intermediárias que irão compor o resultado da função. Observe que este código tem trechos um tanto repetitivos para calcular o primeiro e o segundo dígitos. Você deverá reescrever este código, criando uma função auxiliar que será chamada 2 vezes dentro de isCpfOk. Nessa função auxiliar, você deverá usar where para definir subexpressões.
import Data.Char

isCpfOk :: [Int] -> Bool
isCpfOk cpf = 
  let -- calcula primeiro digito
      digitos1 = take 9 cpf
      dv1 = aux digitos1 10

      -- calcula segundo digito
      digitos2 = digitos1 ++ [dv1]
   in dv1 == cpf !! 9 && (aux digitos2 11) == cpf !! 10

main = do
  let cpf = "12345678909"
      digitos = (map digitToInt cpf)
      result = isCpfOk digitos
  putStrLn (show result)

aux :: [Int] -> Int -> Int
aux list n = if expr < 2 then 0 else 11-expr 
  where expr = (sum $ zipWith (*) list [n, (n-1)..2]) `mod` 11