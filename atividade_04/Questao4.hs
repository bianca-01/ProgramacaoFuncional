module Questao4 where

--função que retorna o resultado da equacao para cada valor x
equacao(x) = (2 * (x ^4)) + (4 * (x ^ 3) + (x * 2)) 

--função recursiva com pendencia que calcula o somatorio da equacao do intervalo (n,m)
somatorio(n,m)
 |n==m = equacao(n)
 |n < m = equacao(n) + somatorio(n+1,m)

--função principal que recebe os parametros n e m, e exibe o resultado do somatorio
main = do
        putStr "N = "
        n <- getLine
        putStr "M = "
        m <- getLine
        putStr "Somatorio = "
        print(somatorio(read n :: Int, read m :: Int))

{-EXEMPLO
  ENTRADA = 2 5
  RETORNO = 2880
-}