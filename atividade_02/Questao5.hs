module Questao5 where
--Questao 05
--Funcao que calcula o imposto devido
impostoDevido(salarioMensal)
 |base < 30000 = 0
 |(base >= 30000) && (base < 40000) = base * 0.075
 |(base >= 40000) && (base < 50000) = base * 0.015
 |(base >= 50000) && (base < 60000) = base * 0.0225
 |base >= 60000 = base * 0.0275
    where base = (salarioMensal*12) - ((salarioMensal*12)*20)/100

--Funcao que calcula a restituicao
restituicao(impostoPago,impostoDevido) = (impostoPago*12) - impostoDevido

--Funcao principal
main = do
        putStr("Salario bruto mensal: ")
        sm <- getLine
        putStr("Imposto mensal pago: ")
        imp <- getLine
        putStr("Imposto devido = ")
        putStrLn(show(impostoDevido(read sm :: Float)))
        putStr("Restituição = ")
        putStrLn(show(restituicao((read imp :: Float,impostoDevido(read sm :: Float)))))

{-EXEMPLO
  ENTRADA = 7000 200
  RETORNO = 1848 552
-}