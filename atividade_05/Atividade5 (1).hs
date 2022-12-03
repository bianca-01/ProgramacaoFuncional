module Atividade5 where

--ATIVIDADE 05  

--letra a - funcao que retorna o valor do somatorio da lista
somatorioLista([],soma) = soma
somatorioLista(c:r,soma) = somatorioLista(r,soma+c)


--letra b
--funcao que retorna o quadrado da lista utilizando a funcao somatorioLista
quadradoLista([]) = []
quadradoLista(c:r) = c ^ 2: quadradoLista(r)

--funcao que retorna uma lista com os valores elevados ao quadrado
quadradoLista2([]) = 0
quadradoLista2(c:r) = somatorioLista(c:r,0) ^ 2

--letra c
--funcao que retorna o maior valor da lista
maior([],m) = m
maior(c:r,m)
 |m < c = maior(r,c)
 |otherwise = maior(r,m)

--letra d
--funcao que retorna em lista, os multiplos de 3
mult3Lista([]) = []
mult3Lista(c:r)
 |(mod c 3 == 0) = c: mult3Lista(r)
 |otherwise = mult3Lista(r) 

--letra e
--funcao que retorna o produto das posições impares da lista
produtoPosImpares([]) = 1
produtoPosImpares([a]) = a
produtoPosImpares(a:b:r) = a * produtoPosImpares(r)

--letra f
--funcao que retorna a soma dos multiplos de 5 da lista
somaMult5([],s) = s
somaMult5(c:r,s)
 |mod c 5 == 0 = somaMult5(r,s+c)
 |otherwise = somaMult5(r,s)

--menu
{- -As funcoes opcoes e menu, formam um loop com as opcoes para a lista.
   -A funcao opcoes recebe a lista como parametro e apresenta as funcoes para o usuario escolher.
   -Apos o usuario digitar a opcao escolhida, é chamada a funçao menu que vai retornar o resultado da função escolhida.
   -Assim que o resultado for apresentado na tela, é chamada novamente a funçao opcoes, ate ser escolhida a condiçao de parada (numero 7).
-}
opcoes(c:r) = do
                putStrLn "*-*-*- MENU *-*-*"
                putStrLn "1- Somatório dos elementos da Lista."
                putStrLn "2- Quadrado dos elementos da Lista."
                putStrLn "3- Maior elemento da Lista."
                putStrLn "4- Múltiplos de 3 da Lista."
                putStrLn "5- Produto das Posições Impares da Lista."
                putStrLn "6- Somatório dos múltiplos de 5 da Lista."
                putStrLn "7- Sair"
                putStr "-> "
                x <- getLine
                menu(read x :: Int,c:r)

menu(x,c:r)
 |x == 1 = do
            putStr "Somatorio da lista = " >> putStrLn (show(somatorioLista(c:r,0))) 
            opcoes(c:r)
 |x == 2 = do
            putStr "Quadrado da lista = " >> putStrLn (show(quadradoLista2(c:r))) 
            putStr "Quadrado de cada elemento da lista = " >> putStrLn (show(quadradoLista(c:r))) 
            opcoes(c:r)
 |x == 3 = do
            putStr "Maior valor da lista = " >> putStrLn (show(maior(c:r,0)))
            opcoes(c:r)
 |x == 4 = do
            putStr "Multiplos de 3 = " >> putStrLn (show(mult3Lista(c:r)))
            opcoes(c:r)
 |x == 5 = do
            putStr "Produto das posições impares da lista = " >> putStrLn (show(produtoPosImpares(c:r)))
            opcoes(c:r)
 |x == 6 = do
            putStr "Somatório dos múltiplos de 5 da Lista = " >> putStrLn (show(somaMult5(c:r,1)))
            opcoes(c:r)
 |x == 7 =  putStrLn "Programa Encerrado!"


lerLista = do
            putStrLn "Digite a lista:  "
            numero <- getLine
            return(read numero :: [Int]) 

main = do
        l <- lerLista
        opcoes(l)
            
--EXEMPLO
{-ENTRADA = [2,5,10,12,20,32]
  RETORNO
  1 Somatorio da lista = 81
  2 Quadrado da lista = 6561
    Quadrado de cada elemento da lista = [4,25,100,144,400,1024]
  3 Maior valor da lista = 32
  4 Multiplos de 3 = [12]
  5 Produto das posições impares da lista = 400
  6 Somatório dos múltiplos de 5 da Lista = 36
  7 Programa Encerrado!
-}       




