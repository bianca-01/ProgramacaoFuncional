module Questao1 where
--Função principal para executar o programa: main

--letra a
somaLista([],s) = s
somaLista(c:r,s) = somaLista(r,s+c)

--funcao que retorna o somatorio das listas utilizando a funcao somaLista
somatorioListas(c1:r1,c2:r2) = somaLista(c1:r1,0) + somaLista(c2:r2,0)

--letra b
--funcao que concatena e ordena as duas listas
uniaoLista([],[]) = []
uniaoLista(l1,[]) = l1
uniaoLista([],l2) = l2
uniaoLista(c1:r1,c2:r2)
 |c1 == c2 = c1: uniaoLista(r1,r2)
 |c1 < c2 = c1: uniaoLista(r1,c2:r2)
 |otherwise = c2: uniaoLista(c1:r1,r2)

--funcao que retorna os elementos da lista elevados ao quadrado, e ordena utilizando a funcao uniaoLista
quadradoListas(l1,l2) = uniaoLista((map (^2) l1),(map (^2) l2))

--letra c 
--funcao que retorna o maior valor das duas listas
maior(l1,l2)
 |maximum(l1) > maximum(l2) = maximum(l1)
 |otherwise = maximum(l2)

--letra d
--funcao que retorna a intersecççao das listas
interListas(l1,l2) = [x | x <- l1, x `elem` l2]

--letra e
--funcao que calcula o produto de dois elementos das listas, de acordo com a posição
produtoPosListas(l1,l2,p) = (l1 !! p) * (l2 !! p)

--letra f
multiplos5 l1 = [x | x <- l1, mod x 5 == 0]

--funcao que retorna os multiplos de 5 das duas listas e ordena utilizando a funcao uniaoLista
mult5Lista(l1,l2) = uniaoLista(multiplos5(map (^2) l1),multiplos5(map (^3) l2))


--menu
{- -As funcoes opcoes e menu, formam um loop com as opcoes para a lista.
   -A funcao opcoes recebe a lista como parametro e apresenta as funcoes para o usuario escolher.
   -Apos o usuario digitar a opcao escolhida, é chamada a funçao menu que vai retornar o resultado da função escolhida.
   -Assim que o resultado for apresentado na tela, é chamada novamente a funçao opcoes, ate ser escolhida a condiçao de parada (numero 7).
-}
opcoes(c1:r1,c2:r2) = do
                putStrLn "*-*-*- MENU *-*-*"
                putStrLn "1- Somatório das Listas."
                putStrLn "2- Quadrado das Listas."
                putStrLn "3- Maior elemento das Listas."
                putStrLn "4- Intersecção das Listas,"
                putStrLn "5- Produto de uma Posição das Listas."
                putStrLn "6- Múltiplos de 5 da Lista1ˆ2 e da Lista2ˆ3."
                putStrLn "7- Sair"
                putStr "-> "
                x <- getLine
                menu(read x :: Int,c1:r1,c2:r2)

menu(x,c1:r1,c2:r2)
 |x == 1 = do
            putStr "Somatorio das listas = " >> putStrLn (show(somatorioListas(c1:r1,c2:r2))) 
            opcoes(c1:r1,c2:r2)
 |x == 2 = do
            putStr "Quadrado de cada elemento das listas = " >> putStrLn (show(quadradoListas(c1:r1,c2:r2))) 
            opcoes(c1:r1,c2:r2)
 |x == 3 = do
            putStr "Maior valor das listas = " >> putStrLn (show(maior(c1:r1,c2:r2)))
            opcoes(c1:r1,c2:r2)
 |x == 4 = do
            putStr "Intersecção das listas = " >> putStrLn (show(interListas(c1:r1,c2:r2)))
            opcoes(c1:r1,c2:r2)
 |x == 5 = do
            putStr "Digite a posiçao: "
            p <- getLine
            putStr "Produto da posição " >> putStr(show(p)) >> putStr " = "  >> putStrLn (show(produtoPosListas(c1:r1,c2:r2,read p :: Int)))
            opcoes(c1:r1,c2:r2)
 |x == 6 = do
            putStr "Múltiplos de 5 da Lista1ˆ2 e da Lista2ˆ3 = " >> putStrLn (show(mult5Lista(c1:r1,c2:r2)))
            opcoes(c1:r1,c2:r2)
 |x == 7 =  putStrLn "Programa Encerrado!"

--funcao para ler uma lista
lerLista = do
            numero <- getLine
            return(read numero :: [Int]) 

--funcao principal
main = do
        putStr "Lista 01: "
        l1 <- lerLista
        putStr "Lista 02: "
        l2 <- lerLista
        opcoes(l1,l2)

{-EXEMPLO 

  ENTRADA 
  Lista 01: [12,34,23,15,18]
  Lista 02: [17,23,20,10,34]

  RETORNO
  1 - Somatorio das listas = 206
  2 - Quadrado de cada elemento das listas = [144,289,529,400,100,1156,529,225,324]
  3 - Maior valor das listas = 34
  4 - Intersecção das listas = [34,23]
  5 - Produto da posição "3" = 150
  6 - Múltiplos de 5 da Lista1ˆ2 e da Lista2ˆ3 = [225,8000,1000]
  7 - Programa Encerrado!

-}