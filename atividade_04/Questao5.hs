module Questao5 where

--função recursiva com pendencia que retorna o produto de dois numeros, utilizando a operaçao de adição
produto(x,y)
 |y == 0 = y
 |otherwise = x + produto(x,y-1)

--função principal que recebe os dois numeros e exibe o resultado da função produto
main = do
        putStr "Num1 = "
        x <- getLine
        putStr "Num2 = "
        y <- getLine
        putStr(show(read x :: Int)) >> putStr " * " >> putStr(show(read y :: Int)) >> putStr " = "
        print(produto(read x :: Int, read y :: Int))

{-EXEMPLO
  ENTRADA = 4 5
  RETORNO = 20
-}