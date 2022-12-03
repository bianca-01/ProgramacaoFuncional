module Questao1 where

--função recursiva que retorna a quantidade de numeros que são multiplos de 3 e nao sao multiplos de 2 e 5.
contaMultiplos3(i,f,cont)
 |(i==f) && (mod i 3 == 0) && (mod i 2 /= 0) && (mod i 5 /= 0) = cont + 1
 | i == f = cont
 |(i < f) && (mod i 3 == 0) && (mod i 2 /= 0) && (mod i 5 /= 0) = contaMultiplos3(i+1,f,cont+1)
 | i < f = contaMultiplos3(i+1,f,cont)
 |otherwise = contaMultiplos3(f,i,cont)

--funcao principal que recebe os valores dos intervalos e chama a funçao contaMultiplos3
main = do
        putStr "Intervalo 1: "
        x <- getLine
        putStr "Intervalo 2: "
        y <- getLine
        putStr("Resultado = ")
        print(contaMultiplos3(read x :: Int, read y :: Int,0))

{-EXEMPLO
  ENTRADA = 2 30
  RETORNO = 4 
-}