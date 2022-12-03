module Questao3 where
--Função principal que apresenta o valor convertido em outra medida
conversor = do
                   putStr"Digite um valor: "
                   x <- getLine
                   putStr"Qual a medida (kg, g, mg) ? "
                   y <- getLine
                   putStr"Qual a medida que você quer converter ? "
                   z <- getLine
                   putStr"RESULTADO: " >> putStr(show(analisa(read x :: Float,y,z))) >> putStr"\n"

--Essa funçao verifica a primeira opcao e retorna a função que ira calcular a conversão 
analisa(x,y,z)
   | y == "kg" = kilo(x,z)
   | y == "g" = grama(x,z)
   | y == "mg" = miligrama(x,z)

--Funçoes que calculam o valor correspondente de cada medida
kilo(kg,op)
   | op == "g" =  kg * 1000 
   | op == "mg" = kg * 1000 * 1000 
   
grama(g,op)
   | op == "kg" =  g / 1000 
   | op == "mg" = g * 1000 
   
miligrama(mg,op)
   | op == "kg" = mg / 1000 / 1000
   | op == "g" = mg / 1000
 
{-EXEMPLO
  ENTRADA = 56 kg g
  RETORNO = 56000
-}