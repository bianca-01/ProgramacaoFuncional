module Atividade5 where

--letra a 
somatorioLista([],soma) = soma
somatorioLista(c:r,soma) = somatorioLista(r,soma+c)

--letra b
quadradoLista([]) = []
quadradoLista(c:r) = c ^ 2: quadradoLista(r)

quadradoLista2([]) = 0
quadradoLista2(c:r) = somatorioLista(c:r,0) ^ 2

--letra c
maior([],m) = m
maior(c:r,m)
 |m < c = maior(r,c)
 |otherwise = maior(r,m)

--letra d
mult3Lista([]) = []
mult3Lista(c:r)
 |(mod c 3 == 0) = c: mult3Lista(r)
 |otherwise = mult3Lista(r) 

--letra e
produtoPosImpares([]) = 1
produtoPosImpares([a]) = a
produtoPosImpares(a:b:r) = a * produtoPosImpares(r)

--letra f
somaMult5([],s) = s
somaMult5(c:r,s)
 |mod c 5 == 0 = somaMult5(r,s+c)
 |otherwise = somaMult5(r,s)

--menu
opcoes = do
          putStrLn "Opções"
          putStrLn "(a) Somatório dos elementos da Lista."
          putStrLn "(b) Quadrado dos elementos da Lista."
          putStrLn "(c) Maior elemento da Lista."
          putStrLn "(d) Múltiplos de 3 da Lista."
          putStrLn "(e) Produto das Posições Impares da Lista."
          putStrLn "(f) Somatório dos múltiplos de 5 da Lista."
          putStrLn "(g) Sair"
          putStr "-> "
          {-x <- getLine
          menu(x)

main = do
        putStr "Digite a lista"-}
imprimir(c) = putStr(show(c))
imprimir(c:r) = do
                 putStr(show(c)) >> putStr " "
                 imprimir(r)

intList(c:r) = map read [c:r] :: [Int]

ler_lista = do
             putStr "Lista = "
             a <- getLine
             show(intList(a:))
            
            
          




