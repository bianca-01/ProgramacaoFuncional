module Programa06 where
import Data.List

notinter(l1,l2) = [x | x <- l1,not(x `elem` l2)]

uniaoLista(l1,l2) = sort l1 ++ l2

funcao1(l1,l2) = uniaoLista(notinter(l1,l2),notinter(l2,l1))

quadradoListas(l1,l2) = uniaoLista((map (^2) l1),(map (^2) l2))

cuboposlista(l1,l2) = ((l1 !! 0) ^ 3) + ((l2 !! 0) ^ 3)

funcao2(l1,l2) = [x | x <- quadradoListas(l1,l2), x > cuboposlista(l1,l2)]

somaLista([],s) = s
somaLista(c:r,s) = somaLista(r,s+c)

lerLista = do
            numero <- getLine
            return(read numero :: [Int]) 

main = do
        putStr "Lista 01: "
        l1 <- lerLista
        putStr "Lista 02: "
        l2 <- lerLista
        putStr "União ordenada entre (A - B) e (B - A) = " >> putStrLn(show(funcao1(l1,l2)))
        putStr "Quadrados das listas que foram maiores que a soma do cubo dos dois elementos = "  >> putStrLn(show(funcao2(l1,l2)))
        putStr "Somatorio dos quadrados das listas que foram maiores que a soma do cubo dos dois elementos =  " >> putStrLn(show(somaLista(funcao2(l1,l2),0)))

