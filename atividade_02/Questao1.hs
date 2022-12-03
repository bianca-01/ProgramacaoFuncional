module Questao1 where
--Questao 01

--Essa função verifica se as retas possuem a condição necessaria para formar um triangulo
triangulo(a,b,c)
 |((a+b > c) && (b+c > a) && (c+a > b)) = True
 |otherwise = False

--Caso as retas não formem um triangulo, a função abaixo exibe qual lado não está dentro da reta 
maior(a,b,c)
 | a > (b+c) = putStrLn(show(a))
 | b > (a+c) = putStrLn(show(b))
 | c > (a+b) = putStrLn(show(c))
 | otherwise = putStrLn (" ")

{-Essa é a função principal, que apresenta o retorno das funções triangulo e maior, se houver o retorno
True a função maior irá retornar um espaço em branco -}
main = do
        putStr("A: ")
        a <- getLine
        putStr("B: ")
        b <- getLine
        putStr("C: ")
        c <- getLine
        putStrLn(show(triangulo(read a :: Int,read b :: Int,read c :: Int)))
        maior(read a :: Int,read b :: Int,read c :: Int)     

{- EXEMPLO
   ENTRADA = 5, 15, 2O
   RETORNO = True
-}