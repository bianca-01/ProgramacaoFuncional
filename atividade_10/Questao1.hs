module Questao1 where
import Data.List
--QUESTÃO 01
--Função para executar o programa no ghci -> main

--MAIN
main :: IO ()
main = do
        putStr "Lista 01: "
        l1 <- lerLista
        putStr "Lista 02: "
        l2 <- lerLista
        opcoes(l1,l2)

--funções que formam o menu do programa
opcoes :: ([Int], [Int]) -> IO ()
opcoes(l1,l2) = do
                putStrLn "=========================================" 
                putStrLn "*-*-*- MENU *-*-*"
                putStrLn "1- Somatório das Listas."
                putStrLn "2- Quadrado das Listas."
                putStrLn "3- Maior elemento das Listas."
                putStrLn "4- Múltiplos de 3 das Listas."
                putStrLn "5- Produto da Posição das Listas."
                putStrLn "6- Múltiplos de 5 da Lista1ˆ2 e da Lista2ˆ3."
                putStrLn "7- Reiniciar o programa"
                putStrLn "8- Sair"
                putStrLn "=========================================" 
                putStr "-> "
                x <- getLine
                menu(read x :: Int,l1,l2)

menu :: (Int, [Int], [Int]) -> IO ()
menu(x,l1,l2)
 |x == 1 =  do
            putStrLn "=========================================" 
            putStrLn "Somatório das Listas: " >> print(somaLista(l1,l2))
            putStrLn "=========================================" 
            opcoes(l1,l2)

 |x == 2 = do
            putStrLn "=========================================" 
            putStrLn "Quadrado das Listas: " >> print(quadradoListas(l1,l2))        
            putStrLn "=========================================" 
            opcoes(l1,l2)

 |x == 3 = do
            putStrLn "=========================================" 
            putStrLn "Maior elemento das Listas: " >> print(maior(l1,l2))
            putStrLn "=========================================" 
            opcoes(l1,l2)


 |x == 4 = do
            putStrLn "=========================================" 
            putStrLn "Múltiplos de 3 das Listas: " >> print(multiplos3(l1,l2))
            putStrLn "=========================================" 
            opcoes(l1,l2)

 |x == 5 =  do
            putStrLn "=========================================" 
            putStr "Digite a posicao: "
            p <- getLine 
            putStrLn "Produto da Posição das Listas: " >> print(produtoPos(l1,l2, read p::Int))
            putStrLn "=========================================" 
            opcoes(l1,l2)

 |x == 6 = do
            putStrLn "=========================================" 
            putStrLn "Múltiplos de 5 da Lista1ˆ2 e da Lista2ˆ3: " >> print(multiplos5(l1,l2))
            putStrLn "=========================================" 
            opcoes(l1,l2)

 |x == 7 = do
            putStrLn "Reiniciando..."
            main

 |x == 8 = putStrLn "Programa Encerrado"

lerLista :: IO [Int]
lerLista = do
            lista <- getLine 
            return (read lista :: [Int])

--letra a
somaLista :: ([Int],[Int]) -> Int
somaLista(l1,l2) = sum (l1++l2)

--letra b
quadradoListas :: ([Int],[Int]) -> [Int]
quadradoListas(l1,l2) = map (^2) l1 ++ map (^2) l2

--letra c
maior :: ([Int], [Int]) -> Int
maior(l1,l2) = maximum (sort l1++l2)

--letra d
multiplos3 :: ([Int], [Int]) -> [Int]
multiplos3(l1,l2) = filter p (sort l1++l2)
                    where p x = x `mod` 3 == 0

--letra e
produtoPos :: ([Int], [Int], Int) -> Int 
produtoPos(l1,l2,p) = (l1 !! p) * (l2 !! p)

--letra f
multiplos5 :: ([Int], [Int]) -> [Int]
multiplos5(l1,l2) = filter p (sort (map (^2) l1)++(map (^3) l2))
                    where p x = x `mod` 5 == 0

{-EXEMPLO
 -ENTRADA:
  Lista 01: [1,2,3,5]
  Lista 02: [10,14,131,200]
 -RETORNO
  opcoes:
 -1 Somatório das Listas: 366
 -2 Quadrado das Listas: [1,4,9,25,100,196,17161,40000]
 -3 Maior elemento das Listas: 200
 -4 Múltiplos de 3 das Listas: [3]
 -5 Digite a posicao: 3
    Produto da Posição das Listas: 1000
 -6 Múltiplos de 5 da Lista1ˆ2 e da Lista2ˆ3: [25,1000,8000000]
 -7 reinicia o programa
 -8 encerra o programa    
-}