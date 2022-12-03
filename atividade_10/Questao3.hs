module Questao3 where
import Data.List
--QUESTAO 03
--Função para executar o programa no ghci -> main

--tipos
type CPF = Integer
type Nome = String 
type Sexo = String
type Dia = Integer
type Mes = Integer
type Ano = Integer
type DtNas = (Dia,Mes,Ano)
type Pessoa = (CPF,Nome, DtNas, Sexo)

--main
main :: IO ()
main = menu([])

menu :: [Pessoa] -> IO()
menu(lista) = do
                print "1- Cadastrar Pessoa"
                print "2- Pesquisar Pessoa"
                print "3- Imprimir todas as pessoas"
                print "4- Sair"
                putStr "-> "
                op <- getLine
                opcoes(lista,read op :: Int)

opcoes :: ([Pessoa], Int) -> IO ()
opcoes(lista,op)
 |op == 1 = do
             p <- lerPessoa
             let lista2 = lista++[p]
             menu(lista2)

 |op == 2 = do
            cpf <- lerCpf
            busca(cpf,lista)
            menu(lista)

 |op == 3 = do
             imprimirListaPessoa(lista)
             menu(lista)

 |op == 4 = putStrLn "Programa Encerrado"



--funcoes para ler os tipos
lerPessoa :: IO (CPF, Nome, (Dia, Mes, Ano), Sexo)
lerPessoa = do
              putStrLn "Digite os dados da pessoa:"
              cpf <- lerCpf
              n <- lerNome
              d <- lerData
              s <- lerSexo
              let pes = (cpf, n, d, s)
              return pes

lerCpf :: IO CPF
lerCpf = do
           putStr "Digite o CPF: "
           cpf <- getLine
           if length cpf == 11
               then return (read cpf :: CPF)
               else do
                    putStrLn "CPF invalido, tente novamente"
                    lerCpf

lerNome :: IO Nome
lerNome = do
            putStr "Digite o nome (com aspas, ex: \"Joao\"): "
            n <- getLine
            return (read n :: Nome)

lerSexo :: IO Sexo
lerSexo = do
            putStr "Digite o sexo (com as aspas: \"feminino\", \"masculino\", \"outro\"): "
            s <- getLine
            if (s == "\"feminino\"")||(s == "\"masculino\"")||(s == "\"outro\"")
                then return (read s :: Sexo)
                else do
                       putStrLn "Opcao invalida, tente novamente"
                       lerSexo


lerData :: IO (Dia, Mes, Ano)
lerData = do
            putStrLn "Data de nascimento"
            putStr "Digite o dia: "
            d <- getLine
            putStr "Digite o mes: "
            m <- getLine
            putStr "Digite o ano: "
            a <- getLine
            if validarData(read d :: Dia, read m :: Mes, read a :: Ano)
                then return (read d :: Dia, read m :: Mes, read a :: Ano)
                else do
                     putStrLn "Data invalida, tente novamente"
                     lerData

--funcao para validar a data de nascimento    
validarData :: (Dia, Mes, Ano) -> Bool            
validarData(d,m,a)
 |m > 12 = False
 |d > 28 && m==2 = False
 |a > 2020 || a < 1930 = False
 |(d == 31) && ((m == 4) || (m == 6) || (m == 9) || (m == 11)) = False
 |otherwise = True 

--funcao para imprimir uma pessoa
imprimirPessoa :: (CPF,Nome,DtNas,Sexo) -> IO ()
imprimirPessoa(cpf, n, d, s) = do
                                putStrLn "====================================="
                                putStrLn(show(cpf))
                                putStr "Nome: " >> putStrLn(show(n))
                                putStr "Data de nascimento: " >> putStrLn(show(d))
                                putStr "Sexo: " >> putStrLn(show(s))
                                putStrLn "====================================="

--funcao para imprimir a lista de pessoas
imprimirListaPessoa ::[Pessoa] -> IO ()
imprimirListaPessoa([]) = putStrLn "Nao ha pessoas cadastradas"  
imprimirListaPessoa(c:r) = do
                           imprimirPessoa(c)
                           imprimirListaPessoa(r) 

--funcao que imprime o resultado da busca por CPF    
busca :: (CPF, [(CPF, Nome, DtNas, Sexo)]) -> IO ()
busca(cpf,[]) = putStrLn "CPF nao encontrado"
busca(cpf,(c, n, d, s):r)
 |c == cpf = imprimirPessoa(c, n, d, s)
 |otherwise = busca(cpf,r)        

 