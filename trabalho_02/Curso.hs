module Curso(
    Cod1,
    Nome,
    Periodos,
    Curso,
    fileCurso,
    listaCurso,
    lerCurso,
    cadastrarCurso,
    codCurso,
    buscaCurso,
    printCurso,
    listarCursos
)where

import Data.Char ( toUpper )
import Data.List ( sort ) 
import System.Directory ( doesFileExist )

--tipos
type Cod1 = Int 
type Nome = String 
type Periodos = Int 
type Curso = (Cod1, Nome, Periodos)

--funcoes
fileCurso :: IO ()
fileCurso = do
                file <- doesFileExist "txt/cursos.txt"
                if not file
                    then writeFile "txt/cursos.txt"  "[]"
                    else putStr ""
                            
listaCurso :: IO [Curso]
listaCurso = do
                curso <- readFile "txt/cursos.txt"
                return (read curso :: [Curso])

lerCurso :: IO Curso
lerCurso = do
            putStr "Codigo do Curso: "
            cod <- getLine 
            lista <- listaCurso
            let busca = buscaCurso(read cod :: Cod1, lista)
            if not (null busca)
                then do 
                        putStrLn "Esse codigo ja esta cadastrado!, tente novamente"
                        lerCurso
                else do
                        putStr "Nome do Curso: "
                        nome <- getLine 
                        putStr "Qnt de periodos: "
                        qnt <- getLine 
                        let curso = (read cod::Cod1,map toUpper nome, read qnt :: Periodos)
                        return curso    

cadastrarCurso :: IO ()
cadastrarCurso = do
                        lista <- listaCurso
                        curso <- lerCurso
                        let lista2 = sort lista++[curso]
                        seq lista (writeFile "txt/cursos.txt" (show(lista2)))
                        putStrLn "Cadastro Realizado!"


codCurso :: Curso -> Cod1
codCurso(cod,_,_) = cod

buscaCurso :: (Cod1, [Curso]) -> [Curso]
buscaCurso(cod,lista) = [x | x <- lista, codCurso x == cod]

printCurso :: Curso -> IO ()
printCurso(cod,nome,qnt) = do
                              putStrLn "==========================="
                              putStr "Codigo: " >> print cod
                              putStr "Curso: " >> print nome
                              putStr "Qnt de periodos: " >> print qnt
                              putStrLn "============================"

listarCursos :: IO ()
listarCursos = do
                lista <- listaCurso
                sequence (map printCurso lista)
                putStrLn ""     