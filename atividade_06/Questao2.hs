module Questao2 where
-- Questao 02

--funçao que le uma lista de inteiros
lerLista = do
            putStr "Digite a lista: "
            numero <- getLine
            return(read numero :: [Int]) 

--essa funçao retorna os elementos pares da lista
paresLista([]) = []
paresLista(c:r)
 |even c = c: paresLista(r)
 |otherwise = paresLista(r)

--essa funçao retorna os elementos pares da lista
imparesLista([]) = []
imparesLista(c:r)
 |odd c = c: imparesLista(r)
 |otherwise = imparesLista(r)

--funcao principal que recebe a lista e retorna o resultado das funçoes paresLista e imparesLista
main = do
        l <- lerLista
        putStr "Elementos pares = " >> putStrLn(show(paresLista(l)))
        putStr "Elementos impares = " >> putStrLn(show(imparesLista(l)))

{-EXEMPLO
  ENTRADA = [12,10,56,13,17,42,21]
  RETORNO = 
  Elementos pares = [12,10,56,42]
  Elementos impares = [13,17,21]
-}
