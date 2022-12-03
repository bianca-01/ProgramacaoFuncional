module Atividade9 where
--Atividade 09
main :: IO ()
main = do
        putStr "Digite a 1º lista: "
        l1 <- lerLista
        putStr "Digite a 2º lista: "
        l2 <- lerLista
        opcoes(l1,l2)


--letra a
tam :: Foldable t => t a -> [Int]
tam(l) = [x | x <-[0..length(l)-1]]

somaLista :: Num a => ([a], [a]) -> [a]
somaLista(l1,l2) = [(l1!!z)+(l2!!z) | z<-[0..length(l1)-1]]

--letra b
notInter :: (Foldable t, Eq a) => ([a], t a) -> [a]
notInter(l1,l2) = [x | x <-l1, not(x `elem` l2)]

--letra c
multPrimItem :: Integral a => ([a], [a]) -> [a]
multPrimItem(l1,l2) = [x | x<-l2, (mod x (l1!!0) == 0)]

--letra d
maiorPrimItem :: (Ord a, Foldable t) => (t a, [a]) -> [a]
maiorPrimItem(l1,l2) = [x | x <- l2, x > maximum(l1)]

--menu
opcoes :: (Show a, Integral a) => ([a], [a]) -> IO ()
opcoes(l1,l2) = do
                putStrLn "*-*-*- MENU *-*-*"
                putStrLn "1- Soma de cada elemento da primeira lista com cada elemento da segunda lista."
                putStrLn "2- Elementos da primeira lista que não estão na segunda lista."
                putStrLn "3- Elementos da segunda lista que são múltiplos do primeiro elemento da primeira lista."
                putStrLn "4- Elementos da segunda lista que são maiores do que o maior elemento da primeira lista."
                putStrLn "5- Sair"
                putStr "-> "
                x <- getLine
                menu(read x :: Int,l1,l2)

menu :: (Show a1, Num a2, Integral a1, Eq a2) => (a2, [a1], [a1]) -> IO ()
menu(x,l1,l2)
 |x == 1 =  do
            putStrLn "Soma de cada elemento da primeira lista com cada elemento da segunda lista:" >> putStrLn(show(somaLista(l1,l2))) 
            opcoes(l1,l2)
 |x == 2 = do
            putStrLn "Elementos da primeira lista que não estão na segunda lista:" >> putStrLn (show(notInter(l1,l2))) 
            opcoes(l1,l2)
 |x == 3 = do
            putStrLn "Elementos da segunda lista que são múltiplos do primeiro elemento da primeira lista." >> putStrLn (show(multPrimItem(l1,l2)))
            opcoes(l1,l2)
 |x == 4 = do
            putStrLn "Elementos da segunda lista que são maiores do que o maior elemento da primeira lista." >> putStrLn (show(maiorPrimItem(l1,l2)))
            opcoes(l1,l2)
 |x == 5 =  putStrLn "Programa Encerrado!"


lerLista :: IO [Int]
lerLista = do
            lista <- getLine
            return(read lista :: [Int]) 
