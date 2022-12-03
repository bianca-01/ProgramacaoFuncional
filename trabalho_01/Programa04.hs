module Programa04 where
import Data.List
import System.Random

num = randomRIO(1,100::Int)

cont(n) = take n (cycle [1,0,2])

pos(l1) = last l1       

lugar1(x) = pos(cont(x))

lugar2(x,y)
 |y == lugar1(x) && y == 1 = 0
 |y == lugar1(x) && y == 0 = 2
 |y == lugar1(x) && y == 2 = 1
 |otherwise = lugar1(x)

lugar3(x,y) = [1,0,2] \\ [x,y]

main = do
        x <- num
        y <- num
        putStr "Ana vai sentar na cadeira " >> putStrLn(show(lugar1(x)))
        putStr "Beatriz vai sentar na cadeira " >> putStrLn(show(lugar2(y,lugar1(x))))
        putStr "Carolina vai sentar na cadeira " >> putStrLn(show(lugar3(lugar1(x),lugar2(y,lugar1(x)))))




        
        