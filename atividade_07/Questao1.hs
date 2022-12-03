module Questao1 where
import Data.Char
--QUESTÃO 01
{-funcao principal que ler a lista e apresenta o resultado das seguintes funcoes:
  maiorStr, tamMaiorStr
  strMenor6
  contStrIniMaiuscula
  strIniMaiuscula
-}
main = do
        putStr "Digite a lista: "
        l <- lerLista
        putStr "Maior String = " >> putStr(show(maiorStr(l))) >> putStr ", tamanho = " >> putStrLn(show(tamMaiorStr(l)))
        putStr "Strings menores que 6 = " >> putStrLn(show(strMenor6(l)))
        putStr"Qnt de strings que iniciam com letra maiuscula = " >> putStrLn((show(contStrIniMaiuscula(l))))
        putStr"Strings que iniciam com letra maiuscula = " >> putStrLn((show(strIniMaiuscula(l))))

--funcao que retorna o tamanho de uma string
tamanho(str) = length str

--essa funcao retorna a maior string 
maiorStr(l) = maximum l

--essa funcao retorna o tamanho da maior string
tamMaiorStr(l) = tamanho(maiorStr(l))

--funcao que retorna as strings menores que 6
strMenor6(l) = [x | x <- l, tamanho(x) < 6]

--funcao que retorna true caso a string inicie com a letra maiuscula, com base nos valores da tabela ascii
caracMaiusculo(c:r)
 |ord c >= 65 && ord c <= 95 = True
 |otherwise = False

--funcao que retorna a quantidade de strings que iniciam com letra maiuscula
contStrIniMaiuscula [] = 0
contStrIniMaiuscula(c:r)
 |caracMaiusculo(c) == True = 1 + contStrIniMaiuscula(r)
 |otherwise = contStrIniMaiuscula(r)

--essa funcao retorna a quantidade de strings que iniciam com letra maiuscula
strIniMaiuscula(l) = [x | x <- l, caracMaiusculo(x)==True]

--funcao para ler a lista de strings
lerLista = do
            strings <- getLine
            return(read strings :: [String]) 

{-EXEMPLO
  ENTRADA: ["bola","Boneca","cachorro","Rio"]
  RETORNO:
  Maior String = "cachorro", tamanho = 8
  Strings menores que 6 = ["bola","Rio"]
  Qnt de strings que iniciam com letra maiuscula = 2
  Strings que iniciam com letra maiuscula = ["Boneca","Rio"]
-}
