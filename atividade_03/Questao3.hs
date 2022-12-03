module Questao3 where

--funcao que retorna o maximo divisor comum (MDC)
mdc(x,y)
 |x < y = mdc(y, x)
 |y == 0 = x
 |otherwise = mdc(y, (mod x y))

--funcao que calcula o mmc utilizando a seguinte formula (x * y)/(mdc(x,y))
mmc(x,y)
 |(x == 0) || (y == 0) = 0
 |x == y = x
 |otherwise = div (x * y) (mdc(x,y))

--funcao que recebe os parametros e retorna o mmc
main = do
        putStr "Num1: "
        x <- getLine
        putStr "Num2: "
        y <- getLine
        putStr "MMC = "
        print(mmc(read x :: Int, read y :: Int))

{-EXEMPLO
  ENTRADA = 12 40
  RETORNO = 120
-}