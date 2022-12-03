module Questao2 where

{- funcao recursiva sem pendencia que retorna a soma entre o produto dos múltiplos de 3 e o quadrado 
da soma dos divisores de 100. -}
soma1(i,f,p,d)
 |(i == f) && (mod i 3 == 0) = (p*i) + (d ^ 2)
 |(i == f) && (mod 100 i == 0) = p + ((d + i) ^ 2)
 |i == f = p + (d ^ 2)
 |(i < f) && (mod i 3 == 0) = soma1(i+1,f,p*i,d)
 |(i < f) && (mod 100 i == 0) = soma1(i+1,f,p,d+i)
 |i < f = soma1(i+1,f, p, d)
 |otherwise = soma1(f,i,p,d)

--funcao principal que recebe os valores dos intervalos e chama a funçao soma1
main = do
        putStr "Intervalo 1: "
        x <- getLine
        putStr "Intervalo 2: "
        y <- getLine
        putStr("Resultado = ")
        print (soma1(read x :: Int,read y :: Int,1,0))

{-EXEMPLO
  ENTRADA = 2 10
  RETORNO = 603
-}