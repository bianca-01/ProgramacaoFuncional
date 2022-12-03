module Questao6 where
--Questao 06
--funcao que calcula o fatorial
fatorial(n)
 |(n == 1) || (n == 0) = 1
 |otherwise = n * fatorial(n-1)

--funcao que retorna o resultado da expressao
exp1(n) = ((fatorial(n) * 3) / (fatorial(n - 4) * 2))

--Funcao recursiva que retorna o somatorio
main(n,m,s)
 | m == n = s + 36
 | n < m = s + main(n+1,m,exp1(n+1))
 