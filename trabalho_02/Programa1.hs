module Programa1 where
import Data.Char
import Data.List (nub)

main :: IO ()
main = do
        putStr "Digite a lista de strings: "
        l <- lerLista
        putStr "Qnt de caracteres não repetidos em cada string: " >> print(contStr(l))
        putStr "Tipos de caracteres que iniciam cada string: " >> print(inicioStr(l))
        putStr "String com a maior qnt de vogais: " >> print(contarVogal(l))


lerLista :: IO [String]
lerLista = do
            strings <- getLine
            return (read strings :: [String]) 

--letra a
contStr :: [String] -> [Int]
contStr(lista) = [length (nub x) | x<-lista]

--letra b
ehvogal :: Char -> Bool
ehvogal(c)
 |c=='a' || c=='e' || c == 'i' || c=='o' || c == 'u' = True
 |otherwise = False

tipo :: String -> String 
tipo(c:_)
 |isDigit c = "digito"
 |ehvogal(toLower(c)) && (isUpper c) = "vogal maiuscula"
 |ehvogal(toLower(c)) && (isLower c) = "vogal minuscula"
 |isUpper c = "consoante maiuscula"
 |isLower c = "consoante minuscula"
 |isSpace c = "espaço"
 |isPunctuation c = "pontuacao"
 |isSymbol c = "simbolo"

inicioStr :: [String] -> [String]
inicioStr(lista) = [tipo(x) | x<-lista]

--letra c
contarVogal :: [String] -> (Int,String)
contarVogal(lista) = maximum [(length [s | s <-str, ehvogal(toLower s)],str) | str <- lista]


