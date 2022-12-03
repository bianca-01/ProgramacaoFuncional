module Programa01 where

maior(l) = maximum(l)

verificaQnt(i,m)
 |m > 0 = putStr(show(n)) >> putStr(" aparece ") >> putStr(show(i)) >> putStrLn"x " 
 |otherwise = putStr ""

fatorar1(x,i,c)
 |not(mod x i == 0) = return (x,c)
 |otherwise = fatorar1(div x i, i,c+1)

fatorar2(a,b,c,i)
 |not((a == 1) && (b == 1) && (c == 1)) = do
                                            f1 <- fatorar1(a,i,0)
                                            f2 <- fatorar1(b,i,0)
                                            f3 <- fatorar1(c,i,0)
                                            verificaQnt(i,maior([snd f1, snd f2, snd f3]))
                                            fatorar2(fst f1, fst f2, fst f3, i+1)
 |otherwise = putStrLn" "

main = do
        putStr "N1: "
        n1 <- getLine
        putStr "N2: "
        n2 <- getLine
        putStr "N3: "
        n3 <- getLine
        putStrLn "Fatoração dos 3 numeros: "
        fatorar2(read n1 :: Int,read n2 :: Int,read n3 :: Int,2)