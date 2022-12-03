module Disciplina(
     Cod2,
     Periodo,
     Disciplina,
     fileDisciplina,
     listaDisciplina,
     lerDisciplina,
     cadastrarDiscp,
     printDisciplina,
     listarDiscip,
     codDisciplina,
     cursoDisciplina,
     perDisciplina,
     buscaDisciplina,
     disciplinaCurso,
     disciplinaPer
)where
import Data.Char ( toUpper )
import Data.List ( sort )
import Curso ( buscaCurso, cadastrarCurso, Cod1, Nome, listaCurso ) 
import System.Directory ( doesFileExist )
--tipos
type Cod2 = Int 
type Periodo = Int 
type Disciplina = (Cod2,Cod1, Nome, Periodo)
--funcoes
fileDisciplina :: IO ()
fileDisciplina = do
                    file <- doesFileExist "txt/disciplinas.txt"
                    if not file
                         then writeFile "txt/disciplinas.txt" "[]"
                         else putStr ""

listaDisciplina :: IO [Disciplina]
listaDisciplina = do
                    discip <- readFile "txt/disciplinas.txt"
                    return (read discip :: [Disciplina])

lerDisciplina :: IO Disciplina
lerDisciplina = do
                putStr "Codigo da disciplina:  "
                cod <- getLine 
                putStr "Codigo do curso: "
                curso <- getLine 
                cursos <- listaCurso 
                discip <- listaDisciplina
                let result = buscaCurso(read curso :: Cod1, cursos)
                let busca = buscaDisciplina(read cod :: Cod2,discip)
                if (null result) || not(null busca)
                    then do
                         if null result
                         then do
                              putStrLn "Erro! curso não cadastrado"
                              putStr "Deseja cadastrar o curso?\n1-Sim\n2-Não\n-> "
                              op <- getLine 
                              if op == "1"
                                   then do
                                        cadastrarCurso 
                                        putStrLn "Cadastre a disciplina: "
                                        lerDisciplina
                                   else lerDisciplina
                         else do
                              putStrLn "Disciplina ja cadastrada, tente novamente"
                              lerDisciplina
                else do
                    putStr "Curso verificado: " >> print result
                    putStr "Nome da disciplina: "
                    nome <- getLine 
                    putStr "Periodo: "
                    per <- getLine 
                    let d = (read cod :: Cod2, read curso :: Cod1, map toUpper nome , read per :: Periodo)
                    return d

cadastrarDiscp :: IO ()
cadastrarDiscp = do
                    dsc <- lerDisciplina
                    lista <- listaDisciplina
                    let lista2 = sort lista++[dsc]
                    seq lista (writeFile "txt/disciplinas.txt" (show(lista2)))
                    putStrLn "Cadastro Realizado"

printDisciplina :: Disciplina -> IO ()
printDisciplina(cod,curso,nome,per) = do
                                       putStrLn "============================"
                                       putStr "Codigo da disciplina: " >> print cod
                                       putStr "Codigo do curso: " >> print curso
                                       putStr "Disciplina: " >> print nome
                                       putStr "Periodo " >> print per
                                       putStrLn "============================"
listarDiscip :: IO ()
listarDiscip = do
                 lista <- listaDisciplina
                 sequence (map printDisciplina lista)
                 putStrLn ""

codDisciplina :: Disciplina -> Cod2
codDisciplina(cod,_,_,_) = cod 

cursoDisciplina :: Disciplina -> Cod1
cursoDisciplina(_,curso,_,_) = curso

perDisciplina :: Disciplina -> Periodo
perDisciplina(_,_,_,per) = per

buscaDisciplina :: (Cod2, [Disciplina]) -> [Disciplina]
buscaDisciplina(cod,lista) =  [x | x <- lista, codDisciplina x == cod]

disciplinaCurso :: (Cod1, [Disciplina]) -> IO ()
disciplinaCurso(curso,lista) = do
                                   let result = [x | x <- lista, cursoDisciplina x == curso] 
                                   if null result
                                        then putStrLn "Não há disciplinas cadastradas nesse curso"  
                                        else do
                                             sequence (map printDisciplina result)
                                             putStrLn "" 

disciplinaPer :: (Periodo, [Disciplina]) -> IO ()
disciplinaPer(per,lista) = do
                                   let result = [x | x <- lista, perDisciplina x == per] 
                                   if null result
                                        then putStrLn "Não há disciplinas cadastradas nesse periodo"  
                                        else do
                                             sequence (map printDisciplina result)
                                             putStrLn ""  