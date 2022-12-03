module Questao2 where

--função recursiva com pendencia que retorna a soma dos multiplos de 5
somaMultiplos5(i,f)
 |(i==f) && (mod i 5 == 0) = i
 |i == f = 0
 |(i < f) && (mod i 5 == 0) = i+somaMultiplos5(i+1,f)
 | i <f = somaMultiplos5(i+1,f)
 |otherwise = somaMultiplos5(f,i)

--função recursiva com pendencia que retorna a soma dos divisores de 50
somaDivisores50(i,f)
 |(i==f) && (mod 50 i == 0) = i
 |i == f = 0
 |(i<f) && (mod 50 i == 0) = i + somaDivisores50(i+1,f)
 |i<f = somaDivisores50(i+1,f)
 |otherwise = somaDivisores50(f,i)

--função que calcula o produto da soma dos multiplos de 5 e do cubo da soma dos divisores de 50
produto(i,f) = somaMultiplos5(i,f) * (somaDivisores50(i,f) ^ 3)

--função principal que recebe os valores dos intervalos e exibe o resultado da função produto
main = do
        putStr "Intervalo 1: "
        x <- getLine
        putStr "Intervalo 2: "
        y <- getLine
        putStr("Resultado = ")
        print(produto(read x :: Int, read y :: Int))

{-EXEMPLO
  ENTRADA = 2 10
  RETORNO = 73695
-}