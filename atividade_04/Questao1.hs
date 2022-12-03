module Questao1 where

{- função recursiva com pendencia que retorna a quantidade de numeros que sao multiplos de 7 e que 
nao sao multiplos de 2 e 5 -}
contaMultiplos7(i,f)
 |(i==f) && (mod i 7 == 0) && (mod i 2 /= 0) && (mod i 5 /= 0) = 1
 | i == f = 0
 |(i < f) && (mod i 7 == 0) && (mod i 2 /= 0) && (mod i 5 /= 0) = 1 + contaMultiplos7(i+1,f)
 | i < f = contaMultiplos7(i+1,f)
 |otherwise = contaMultiplos7(f,i)

-- função principal que recebe os valores e exibe o resultado da funçao contaMultiplos7
main = do
        putStr "Intervalo 1: "
        x <- getLine
        putStr "Intervalo 2: "
        y <- getLine
        putStr("Resultado = ")
        print(contaMultiplos7(read x :: Int, read y :: Int))

{-EXEMPLO
  ENTRADA = 2 50
  RETORNO = 3
-}