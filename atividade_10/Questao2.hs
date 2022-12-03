module Questao2 where
import Data.Char
--QUESTÃO 02
--Função para executar o programa no ghci -> main

--MAIN
main :: IO ()
main = do
        putStrLn "Digite a lista com as strings: "
        l <- lerLista
        opcoes(l)
       
lerLista :: IO [String]
lerLista = do
            lista <- getLine
            return (read lista :: [String]) 

opcoes :: [String] -> IO ()
opcoes(lista) = do
                putStrLn "=========================================" 
                putStrLn "*-*-*- MENU *-*-*"
                putStrLn "1- Tamanho das strings"
                putStrLn "2- Menor string"
                putStrLn "3- Uniao maior e menor string"
                putStrLn "4- Strings que iniciam com vogal"
                putStrLn "5- Reiniciar"
                putStrLn "6- Sair"
                putStrLn "=========================================" 
                putStr "->"
                x <- getLine
                menu(read x::Int, lista) 

menu :: (Int, [String]) -> IO ()
menu(x, lista)
 |x == 1 = do
            putStrLn "========================================="
            putStr "Tamanho das strings: " >> print(tamString(lista))
            putStrLn "========================================="
            opcoes(lista)

 |x == 2 = do
            putStrLn "========================================="
            putStr "Menor string: " >> print(menor(lista))
            putStrLn "========================================="
            opcoes(lista)

 |x == 3 = do
            putStrLn "========================================="
            putStr "Uniao menor e maior string: " >> print(maiorMenor(lista))
            putStrLn "========================================="
            opcoes(lista)

 |x == 4 = do
            putStrLn "========================================="
            putStr "Strings que iniciam com vogal: " >> print(strVogal(lista))
            putStrLn "========================================="
            opcoes(lista)

 |x == 5 = main

 |x == 6 = putStrLn "Programa Encerrado"


--letra a
tamString :: [String] -> [Int]
tamString(lista) = [length x | x <- lista]

--letra b
menor :: [String] -> String
menor(lista) = minimum(lista)

--letra c
maiorMenor :: [String] -> String 
maiorMenor(lista) = maximum(lista) ++ menor(lista)

--letra d
ehVogal :: [Char] -> Bool
ehVogal(c:r)
 |c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u' = True 
 |otherwise = False 

strVogal :: [String] -> [String]
strVogal(lista) = filter p (lista)
                    where p c = ehVogal(c)

{-EXEMPLO
 -ENTRADA: ["peace","evermore","really","whistle","red","all","anyone"]
 -RETORNO:
  opcoes:
  -1 Tamanho das strings: [5,8,6,7,3,3,6]
  -2 Menor string: "all"
  -3 Uniao menor e maior string: "evermoreall"
  -4 Strings que iniciam com vogal: ["evermore","all","anyone"]
  -5 reinicia o programa
  -6 encerra o programa
  
-}        