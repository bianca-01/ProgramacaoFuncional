module Questao4 where
--Questao 04
--Funcao que retorna um valor de acordo com a condição dos numeros
main(a,b)
 |a > b = (a ^ 3) + ((a / b) ^ 2)
 |a < b = (b ^ 2) * (b - a)
 |a == b = sqrt(a) + ((a+b)^2)

{-EXEMPLO
  ENTRADA = 2 6
  RETORNO = 144
-}
