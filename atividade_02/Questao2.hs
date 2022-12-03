module Questao2 where
--Questao 02
--Função que calcula a media dos tres valores
media(x,y,z) = (x+y+z)/3

--Essa funcao compara um numero com a media, e retorna 1 caso seja igual
igual(n, x, y, z)
 | n == media(x, y, z) = 1
 | otherwise = 0

--Soma do resultado das comparações 
igualmedia(x,y,z) = igual(x, x, y, z) + igual(y, x, y, z) + igual(z, x, y, z)

--Funçao principal que apresenta o retorno da funcao igualmedia
main = do
        putStr("X: ")
        x <- getLine
        putStr("Y: ")
        y <- getLine
        putStr("Z: ")
        z <- getLine
        putStrLn(show(igualmedia(read x :: Float, read y :: Float, read z :: Float)))

{-EXEMPLO
  ENTRADA = 3 6 9
  RETORNO = 1
-}        