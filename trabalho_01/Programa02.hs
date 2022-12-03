module Programa02 where

mmc(a,b,i,r)
 |i > b = r
 |mod (a*i) b == 0 = mmc(a,b,b+1,a*i)
 |otherwise = mmc(a,b,i+1,r)

mmc2(a,b) = mmc(a,b,2,a*2)

main = do
        putStr "N1 = "
        n1 <- getLine
        putStr "N2 = "
        n2 <- getLine
        putStr "N3 = "
        n3 <- getLine
        putStr"MMC = " >> putStrLn(show(mmc2(read n1 :: Int,mmc2(read n2 :: Int, read n3 :: Int))))