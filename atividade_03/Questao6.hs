module Questao6 where

--funcao que calcula o segundo somatorio
equacao1(x,y,n,s)
 |y == n = s+((x ^ 2) + (y ^ 3))
 |y < n = equacao1(x,y+1,n,s+((x ^ 2) + (y ^ 3)))

--funcao que calcula o primeiro somatorio
equacao2(m,n,x,y,s)
 |x == m = s+ equacao1(x,y,n,0)
 |x < m = equacao2(m,n,x+1,y,s+equacao1(x,y,n,0))

--funcao principal que recebe os valores de n e m, e retorna o resultado da equacao 
main = do
        putStr "m = "
        m <- getLine
        putStr "n = "
        n <- getLine
        putStr "Resultado = "
        print(equacao2(read m :: Int, read n :: Int, 2, 1, 0))

{-EXEMPLO
  ENTRADA = 4 5
  RETORNO = 820
-}