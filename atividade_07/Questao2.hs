module Questao2 where
import Data.Char
--QUESTÃO 02
--Função principal que ler as duas listas e apresenta o resultado da funcao uniaoLista
main = do
        putStr "Lista 01: "
        l1 <- lerLista
        putStr "Lista 02: "
        l2 <- lerLista
        putStr "Uniao das listas = " >> print(uniaoLista(l1,l2))

--funcao que vai concatenar e ordenar as duas listas
uniaoLista([],[]) = []
uniaoLista(l1,[]) = l1
uniaoLista([],l2) = l2
uniaoLista(c1:r1,c2:r2)
 |c1 == c2 = c1: uniaoLista(r1,r2)
 |c1 < c2 = c1: uniaoLista(r1,c2:r2)
 |otherwise = c2: uniaoLista(c1:r1,r2)

--funcao para ler as listas de strings
lerLista = do
            strings <- getLine
            return(read strings :: [String]) 

{-EXEMPLO
  ENTRADA: ["ab","bd","de"] ["ac","bc","ca"]
  RETORNO: 
  Uniao das listas = ["ab","ac","bc","bd","ca","de"]
-}