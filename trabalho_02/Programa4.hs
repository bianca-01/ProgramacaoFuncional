module Programa4 where
import Curso
import Disciplina
import Aluno 
import Nota
--main
main :: IO ()
main = do
          arquivos
          putStrLn "------------------MENU------------------"
          putStrLn "1- Cadastro"
          putStrLn "2- Pesquisar"
          putStrLn "3- Visualizar dados cadastrados"
          putStrLn "4- Sair"
          putStr "-> "
          op <- getLine 
          -----------------------------------------------------
          if op == "1"
            then cadastro
          -----------------------------------------------------
            else if op == "2"
            then pesquisar
          -----------------------------------------------------
            else if op == "3"
            then imprimir
          -----------------------------------------------------
            else if op == "4"
            then putStrLn "Programa Encerrado"  
          -----------------------------------------------------           
            else do 
                putStrLn "Opção invalida"
                main

arquivos :: IO ()
arquivos = do
            fileCurso 
            fileDisciplina 
            fileAluno 
            fileNotas      

cadastro :: IO ()
cadastro = do
               putStrLn "---------------CADASTRO---------------"
               putStrLn "1- Curso"
               putStrLn "2- Disciplina"
               putStrLn "3- Aluno"
               putStrLn "4- Notas"
               putStrLn "5- Voltar"
               putStr "-> "
               op <- getLine 
            ---------------------------------------------------------
               if op == "1"
                    then do
                            cadastrarCurso
                            cadastro
            --------------------------------------------------------
                    else if op == "2"
                    then do
                            cadastrarDiscp 
                            cadastro
            ----------------------------------------------------------
                    else if op == "3"
                    then do
                            cadastrarAluno 
                            cadastro
            -----------------------------------------------------------
                    else if op == "4"
                    then do
                            cadastrarNota 
                            cadastro
            -----------------------------------------------------------
                    else if op == "5"
                    then main
            ------------------------------------------------------------
                    else do
                            putStrLn "Opcao invalida"
                            cadastro


pesquisar :: IO ()
pesquisar = do
                cursos <- listaCurso 
                disciplinas <- listaDisciplina 
                alunos <- listaAluno 
                notas <- listaNotas 
                putStrLn "------------------------PESQUISAR-----------------------"
                putStrLn "1- Disciplinas de um curso"
                putStrLn "2- Disciplinas de um periodo"
                putStrLn "3- Alunos de um curso"
                putStrLn "4- Alunos de um periodo"
                putStrLn "5- Notas de um aluno"
                putStrLn "6- Voltar"
                putStr "-> "
                op <- getLine 
                --------------------------------------------------------------------
                if op == "1"
                    then do
                        putStr "Digite o codigo do curso: "
                        curso <- getLine 
                        disciplinaCurso(read curso :: Cod1, disciplinas)
                        pesquisar
                ------------------------------------------------------------------
                    else if op == "2"
                    then do 
                        putStr "Digite o periodo: "
                        per <- getLine 
                        disciplinaPer(read per :: Periodo, disciplinas)
                        pesquisar
                --------------------------------------------------------------------
                    else if op == "3"
                    then do 
                        putStr "Digite o codigo do curso: "
                        curso <- getLine 
                        alunosCurso(read curso :: Cod1, alunos)
                        pesquisar
                -------------------------------------------------------------------------
                    else if op == "4"
                    then do
                        putStr "Digite o periodo: "
                        per <- getLine 
                        alunosPeriodo(read per :: Periodo, alunos)
                        pesquisar
                --------------------------------------------------------------------------
                    else if op == "5"
                    then do
                        putStr "Matricula: "
                        mtr <- getLine 
                        notasAluno(read mtr :: Matricula, notas)
                        pesquisar
                    else if op == "6"
                    then main
                    else do
                            putStrLn "Opção invalida"
                            pesquisar

               
imprimir :: IO ()
imprimir = do
             putStrLn "-----------------IMPRIMIR----------------"
             putStrLn "1- Cursos"
             putStrLn "2- Disciplinas"
             putStrLn "3- Alunos"
             putStrLn "4- Notas"
             putStrLn "5- Voltar"
             putStr "-> "
             op <- getLine 
             ------------------------------------------------------------------
             if op == "1"
                then do
                        listarCursos
                        imprimir
             ------------------------------------------------------------------
                else if op == "2"
                then do
                        listarDiscip 
                        imprimir
             -------------------------------------------------------------------
                else if op == "3"
                then do
                        listarAlunos 
                        imprimir
             --------------------------------------------------------------------
                else if op == "4"
                then do 
                        imprimirNotas 
                        imprimir
             ---------------------------------------------------------------------
                else if op == "5"
                then main
                else do
                        putStrLn "Opção invalida"
                        imprimir

