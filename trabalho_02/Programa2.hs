module Programa2 where
import Data.List(sort)

main :: IO ()
main = do
        putStr "Lista 01: "
        l1 <- lerLista
        putStr "Lista 02: "
        l2 <- lerLista
        putStr "União ordenada entre (A - B) e (B - A) = " >> print(uniaoInter(l1,l2))
        let letraB = qdrMaiorCubo(l1,l2)
        putStr "Quadrados das listas que foram maiores que a soma do cubo dos dois elementos = "  >> print(letraB)
        putStr "Somatorio da lista anterior =  " >> print(sum letraB)


lerLista :: IO [Int]
lerLista = do
            lista <- getLine 
            return (sort (read lista :: [Int]))

--letra a
notinter :: ([Int],[Int]) -> [Int]
notinter(l1,l2) = [x | x <- l1, not(x `elem` l2)]

uniaoInter :: ([Int], [Int]) -> [Int]
uniaoInter(l1,l2) = sort (notinter(l1,l2)++notinter(l2,l1))

--letra b
quadradoListas :: ([Int], [Int]) -> [Int]
quadradoListas(l1,l2) = sort (map (^2) l1)++(map (^2) l2)

cuboposlista :: ([Int], [Int]) -> Int 
cuboposlista(l1,l2) = ((l1 !! 0) ^ 3) + ((l2 !! 0) ^ 3)

qdrMaiorCubo :: ([Int], [Int]) -> [Int]
qdrMaiorCubo(l1,l2) = [x | x <- quadradoListas(l1,l2), x > cuboposlista(l1,l2)]
