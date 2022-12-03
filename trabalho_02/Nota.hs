module Nota(
     Nota,
     Notas,
     fileNotas,
     listaNotas,
     lerNotas,
     cadastrarNota,
     printNota,
     imprimirNotas,
     notasAluno,
     notasDisciplina
)where
import Data.Char ( toUpper )
import Data.List ( sort )
import System.Directory ( doesFileExist )
import Disciplina ( buscaDisciplina, cadastrarDiscp, Cod2, listaDisciplina )
import Aluno ( buscaAluno, cadastrarAluno, Matricula, listaAluno ) 
--tipos
type Nota = Float 
type Notas = (Matricula, Cod2, Nota, Nota)
--funcoes
fileNotas :: IO ()
fileNotas = do
               file <- doesFileExist "txt/notas.txt"
               if not file
                  then writeFile "txt/notas.txt" "[]"
                  else putStr ""

listaNotas :: IO [Notas]
listaNotas = do
               nota <- readFile "txt/notas.txt"
               return (read nota :: [Notas])

lerNotas :: IO Notas
lerNotas = do
            putStr "Matricula: "
            mtr <- getLine 
            putStr "Codigo da disciplina: "
            cod <- getLine 
            putStrLn "Verificando Dados..."
            a <- listaAluno
            d <- listaDisciplina 
            let r1 = buscaAluno(read mtr :: Matricula, a)
            let r2 = buscaDisciplina(read cod :: Cod2, d)
            if (null r1) || (null r2)
               then do
                    putStrLn "Erro! Dados nao encontrados"
                    print mtr
                    print cod
                    putStr "Deseja cadastrar algum dado?\n1-Aluno\n2-Disciplina\n3-Aluno e Disciplina\n4-Não,tentar cadastrar outra nota\n-> "
                    op <- getLine 
                    if op == "1"
                         then do
                              cadastrarAluno 
                              putStrLn "Cadastrar notas: "
                              lerNotas
                         else if op == "2"
                         then do
                                cadastrarDiscp 
                                putStrLn "Cadastrar notas: "
                                lerNotas
                         else if op == "3"
                         then do
                                putStrLn "Cadastrar disciplina: " 
                                cadastrarDiscp 
                                putStrLn "Cadastrar aluno: " 
                                cadastrarAluno 
                                putStrLn "Cadastrar notas: " 
                                lerNotas
                         else lerNotas
               else do
                    putStrLn "Dados verificados"
                    putStrLn "NOTAS"
                    putStr "Nota1: "
                    n1 <- getLine 
                    n2 <- lerNota2
                    let notas = (read mtr::Matricula, read cod :: Cod2, read n1 :: Nota,n2)
                    return notas
            
lerNota2 :: IO Nota
lerNota2 = do
            putStr "Deseja cadastrar a nota2? (s/n) "
            op <- getChar  
            putStrLn ""
            if (toUpper op) == 'S'
               then do
                    putStr "Nota2: "
                    n2 <- getLine 
                    return (read n2 :: Nota)
               else if (toUpper op) == 'N'
               then do 
                    let n2 = "-1"
                    return (read n2 :: Nota)
               else do
                    putStrLn "Opcao invalida"
                    lerNota2

cadastrarNota ::  IO ()
cadastrarNota= do
               nota <- lerNotas
               lista <- listaNotas
               let lista2 = sort lista++[nota]
               seq lista (writeFile "txt/notas.txt" (show(lista2)))
               putStrLn "Cadastro Realizado"


printNota :: Notas -> IO ()
printNota(mtr,dc,n1,n2) = do
                             putStrLn "==========================================="
                             putStr "Matricula: " >> print mtr
                             putStr "Disciplina: " >> print dc
                             putStrLn "NOTAS"
                             putStr "Nota1: " >> print n1
                             if n2>(-1)
                                then do
                                        putStr "Nota2: " >> print n2
                                        putStr "Media: " >> print ((n1+n2)/2)
                                else putStr ""
                             putStrLn "============================================="

imprimirNotas :: IO ()
imprimirNotas = do
                    lista <- listaNotas
                    sequence (map printNota lista)
                    putStrLn ""


mtrNotas :: Notas -> Matricula
mtrNotas(mtr,_,_,_) = mtr


disciplinaNotas :: Notas -> Cod2
disciplinaNotas(_,d,__,_) = d


notasDisciplina :: (Cod2, [Notas]) -> IO ()
notasDisciplina(d,lista) = do
                          let result = [x | x <- lista, disciplinaNotas x == d]
                          if null result
                              then putStrLn "Não ha notas cadastradas"
                              else do
                                     sequence (map printNota lista)
                                     putStrLn ""

notasAluno :: (Matricula, [Notas]) -> IO ()
notasAluno(mtr,lista) = do
                          let result = [x | x <- lista, mtrNotas x == mtr]
                          if null result
                              then putStrLn "Não ha notas cadastradas"
                              else do
                                     sequence (map printNota lista)
                                     putStrLn ""

     