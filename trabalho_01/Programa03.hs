module Programa03 where

definirSecoes(custo,totalIng,taxa,ingSecao,s)
 |custo < totalIng = s
 |custo >= totalIng = definirSecoes(custo+taxa,totalIng+ingSecao,taxa,ingSecao,s+1)

ingSecao(preco,capacidade) = preco * capacidade

main = do
        putStr "Digite o custo da peça: "
        custo <- getLine
        putStr "Digite o custo por seção: "
        taxa <- getLine
        putStr "Digite o preco do ingresso: "
        preco <- getLine
        putStr "Digite a capacidade da sala: "
        cap <- getLine
        let valorIng = ingSecao(read preco :: Int, read cap :: Int)
        let secoes = definirSecoes(read custo :: Int,valorIng,read taxa :: Int, valorIng, 1)
        putStr "Qnt de ingressos: " >> putStrLn(show(ingSecao(secoes,read cap :: Int)))
        putStr "Qnt de secoes: " >> putStrLn(show(secoes))
