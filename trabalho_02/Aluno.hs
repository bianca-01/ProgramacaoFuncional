module Aluno(
   Matricula,
   Aluno,
   fileAluno,
   listaAluno,
   lerAluno,
   cadastrarAluno,
   printAluno,
   listarAlunos,
   matricula,
   cursoAluno,
   periodoAluno,
   buscaAluno,
   alunosCurso,
   alunosPeriodo
)where
import Data.Char ( toUpper )
import Data.List ( sort )
import System.Directory ( doesFileExist )
import Curso ( buscaCurso, cadastrarCurso, Cod1, Nome, listaCurso ) 
import Disciplina ( Periodo ) 
--tipos
type Matricula = Int 
type Aluno = (Matricula, Nome, Cod1, Periodo)
--funcoes
fileAluno :: IO ()
fileAluno = do
               file <- doesFileExist "txt/alunos.txt"
               if not file
                  then writeFile "txt/alunos.txt" "[]"
                  else putStr""

listaAluno :: IO [Aluno]
listaAluno = do
                    aluno <- readFile "txt/alunos.txt"
                    return (read aluno :: [Aluno])

lerAluno :: IO Aluno
lerAluno = do
            putStr "Matricula: "
            mtr <- getLine 
            putStr "Nome: "
            nome <- getLine 
            putStr "Codigo do curso: "
            curso <- getLine 
            cursos <- listaCurso 
            let result = buscaCurso(read curso :: Cod1, cursos)
            if null result
               then do
                     putStrLn "Erro! curso não cadastrado"
                     putStr "Deseja cadastrar o curso?\n1-Sim\n2-Não\n-> "
                     op <- getLine 
                     if op == "1"
                        then do
                              cadastrarCurso 
                              putStrLn "Cadastre o aluno: "
                              lerAluno
                        else lerAluno
               else do
                     putStr "Curso verificado: " >> print result
                     putStr "Periodo: "
                     per <- getLine 
                     let aluno = (read mtr :: Matricula, map toUpper nome, read curso :: Cod1,  read per :: Periodo)
                     return aluno

cadastrarAluno ::  IO ()
cadastrarAluno = do
                  aluno <- lerAluno
                  lista <- listaAluno
                  let lista2 = sort lista++[aluno]
                  seq lista (writeFile "txt/alunos.txt" (show(lista2)))
                  putStrLn "Cadastro Realizado"

printAluno :: Aluno -> IO ()
printAluno(mtr,nome,curso,per) = do
                                    putStrLn "========================================="
                                    putStr "Matricula: " >> print mtr
                                    putStr "Nome: " >> print nome
                                    putStr "Curso: " >> print curso 
                                    putStr "Periodo: " >> print per 
                                    putStrLn "========================================="
listarAlunos :: IO ()
listarAlunos = do
                  lista <- listaAluno
                  sequence (map printAluno lista)
                  putStrLn ""

matricula :: Aluno -> Matricula
matricula(mtr,_,_,_) = mtr

cursoAluno :: Aluno -> Cod1
cursoAluno(_,_,cod,_) = cod   

periodoAluno :: Aluno -> Periodo
periodoAluno(_,_,_,per) = per      

buscaAluno :: (Matricula, [Aluno]) -> [Aluno]
buscaAluno(mtr,lista) = [x | x <- lista, matricula x == mtr]

alunosCurso :: (Cod1, [Aluno]) -> IO ()
alunosCurso(curso,lista) = do
                             let result = [x | x <- lista, cursoAluno x == curso]
                             if null result
                                 then putStrLn "Não há alunos cadastrados nesse curso" 
                                 else do
                                       sequence (map printAluno lista)
                                       putStrLn ""


alunosPeriodo :: (Periodo, [Aluno]) -> IO ()
alunosPeriodo(per,lista) = do
                             let result = [x | x <- lista, periodoAluno x == per]  
                             if null result
                                 then putStrLn "Não há alunos cadastrados nesse curso" 
                                 else do
                                       sequence (map printAluno lista)
                                       putStrLn ""