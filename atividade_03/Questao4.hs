module Questao4 where

--funcao que retorna o produtorio
equacao1(n,m,p)
 |n == m = p * ((2 * (n ^ 3)) + (4 * (n ^ 2)) + n)
 |n < m = equacao1(n+1, m, p * ((2 * (n ^ 3)) + (4 * (n ^ 2)) + n))

--funcao principal que recebe os valores de n e m, e retorna o resultado da equacao
main = do
        putStr "N = "
        n <- getLine
        putStr "M = "
        m <- getLine
        putStr "Resultado = "
        print(equacao1(read n :: Int, read m :: Int, 1))

{-EXEMPLO
  ENTRADA = 1 3
  RETORNO = 22134
-}