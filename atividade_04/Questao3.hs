module Questao3 where

--função que retorna o maximo divisor comum (MDC)
mdc(x,y)
 |x < y = mdc(y, x)
 |y == 0 = x
 |otherwise = mdc(y, (mod x y))

--função que calcula o mmc utilizando a seguinte formula (x * y)/(mdc(x,y))
mmc(x,y)
 |(x == 0) || (y == 0) = 0
 |x == y = x
 |otherwise = div (x * y) (mdc(x,y))

{-função recursiva com pendencia que calcula o minimo multiplo comum de quatro numeros, formando uma 
pilha de execucao utilizando a função mmc-}
mmc2(a,b,c,d) = mmc(a,(mmc(b,mmc(c,d))))

--função principal que recebe os quatro numeros e exibe o resultado da função mmc2
main = do
        putStr "Num1 = "
        a <- getLine
        putStr "Num2 = "
        b <- getLine
        putStr "Num3 = "
        c <- getLine
        putStr "Num4 = "
        d <- getLine
        putStr "MMC = "
        print(mmc2(read a :: Int,read b :: Int,read c :: Int,read d :: Int))

{-EXEMPLO
  ENTRADA = 30 42 56 72
  RETORNO = 2520
-}