module Programa3 where
import Data.Char (toUpper)

--tipos
type Nome = String 
type Estado = String 
type Pais = String 
type Ano = Int 
type Time = (Nome, Estado, Pais, Ano)

--main       
main :: IO ()
main = do
        print "Digite a lista de times: "
        lista <- lerLista
        let listaOrdenada = quicksort(lista)
        menu listaOrdenada

--menu
menu :: [Time] -> IO ()
menu lista = do
                putStrLn "----------------MENU----------------"
                putStrLn "1- Buscar um time"
                putStrLn "2- Imprimir os times cadastrados"
                putStrLn "3- Reiniciar o programa"
                putStrLn "4- Sair"
                putStr "-> " 
                op <- getLine
                -------------------------------------------------------
                if op=="1"
                   then do
                        putStr "Digite o nome do time: "
                        t <- getLine 
                        let time = [toUpper x | x <- (read t :: Nome)]
                        busca(time, lista)
                        menu lista
                -------------------------------------------------------
                   else if op == "2"
                   then do 
                        imprimirLista(lista)
                        menu lista
                -------------------------------------------------------
                   else if op == "3"
                   then main
                -------------------------------------------------------
                   else if op == "4"
                   then putStrLn "Programa Encerrado"
                -------------------------------------------------------
                   else do
                        putStrLn "Opcao invalida"
                        menu lista
                -------------------------------------------------------

        
lerLista :: IO [Time]
lerLista = do
            l <- getLine 
            let lista = [upper x | x <- (read l :: [Time])]
            return lista


upper :: Time -> Time
upper(nome,est,pais,ano) = ([toUpper x | x <- nome],[toUpper y | y <- est],[toUpper z | z <- pais],ano)


quicksort::[Time] -> [Time]
quicksort [] = []
quicksort(c:r) = quicksort [x | x <- r,x < c] ++ [c] ++ quicksort [x | x <- r,x >= c]


nomeTime :: Time -> Nome
nomeTime(nome,est,pais,ano) = nome


busca :: (Nome, [Time]) -> IO ()
busca(nome,lista) = do
                       let result = [x | x <- lista, nomeTime x == nome]
                       if not (null result)
                          then print result
                          else putStrLn "O time nao esta cadastrado"

printTime :: Time -> IO ()
printTime(nome,est,pais,ano) = do
                                  putStrLn "=========================================="
                                  putStr "Nome: " >> print nome 
                                  putStr "Estado: " >> print est 
                                  putStr "País: " >> print pais 
                                  putStr "Ano de fundação: " >> print ano
                                  putStrLn "=========================================="

imprimirLista :: [Time] -> IO ()              
imprimirLista(lista) = do
                        sequence (map printTime lista)
                        putStrLn ""
                     