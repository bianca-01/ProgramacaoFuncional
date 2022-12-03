module Questao6 where

--funcao recursiva com pendencia que calcula o segundo somatorio
equacao1(x,y,n)
 |y == n = (x ^ 2) + (y ^ 3)
 |y < n =  (x ^ 2) + (y ^ 3) + equacao1(x,y+1,n)

--funcao recursiva com pendencia que calcula o primeiro somatorio
equacao2(x,y,m,n)
 |x == m = equacao1(x,y,n)
 |x < m = equacao1(x,y,n) + equacao2(x+1, y, m,n)

--funcao principal que recebe os valores de n e m, e retorna o resultado da equacao 
main = do
        putStr "m = "
        m <- getLine
        putStr "n = "
        n <- getLine
        putStr "Resultado = "
        print(equacao2(2,1,read m :: Int, read n :: Int))

{-EXEMPLO
  ENTRADA = 3 2
  RETORNO = 44
-}