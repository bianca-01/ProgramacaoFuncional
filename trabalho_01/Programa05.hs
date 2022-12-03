module Programa05 where
import Data.Char
import Data.List

--letra a
strNaoRep([]) = []
strNaoRep(c:r) = (nub c, length (nub c)): strNaoRep(r)

--letra b
tipo(c:r)
 |isDigit c = "digito"
 |ehvogal(toLower(c)) && (isUpper c) = "vogal maiuscula"
 |ehvogal(toLower(c)) && (isLower c) = "vogal minuscula"
 |isUpper c = "consoante maiuscula"
 |isLower c = "consoante minuscula"
 |isSpace c = "espaço"
 |isPunctuation c = "pontuacao"
 |isSymbol c = "simbolo"

inicioLista([]) = []
inicioLista(c:r) = tipo(c): inicioLista(r)

--letra c 
ehvogal(c)
 |c=='a' || c=='e' || c == 'i' || c=='o' || c == 'u' = True
 |otherwise = False

contaVogais [] = 0
contaVogais(c:r)
 |ehvogal(toLower(c)) = 1 + contaVogais(r)
 |otherwise = contaVogais(r)
 
strVogal [] = []
strVogal(c:r) = (contaVogais(c),c): strVogal(r)

maior(l) = maximum(l)
                  
lerLista = do
            strings <- getLine
            return(read strings :: [String]) 

main = do
        putStr "Digite a lista de strings: "
        l <- lerLista
        putStr "Caracteres não repetidos em cada string: " >> putStrLn(show(strNaoRep(l)))
        putStr "Tipos de caracteres que iniciam cada string: " >> putStrLn(show(inicioLista(l)))
        putStr "String com a maior qnt de vogais: " >> putStrLn(show(maior(strVogal(l))))


