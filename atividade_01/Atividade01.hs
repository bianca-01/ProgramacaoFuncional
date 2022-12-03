--ATIVIDADE 01  
module Atividade01 where

--QUESTAO 01
areaTriangulo(b,h) = (b*h) / 2
{-Essa função calcula a área de um triângulo, tendo dois parametros que serão digitados pelo
usuario: a base (b) e altura (h) do mesmo, o retorno da funcao será o produto da base pela altura, dividido por
dois.
    EXEMPLO:
    ENTRADA: areaTriangulo(4,6)
    RETORNO: 12.0
-}

--QUESTAO 02
quadrado(x) = x ^ 2
cubo(x) = x ^ 3
{- As funçoes quadrado e cubo, retornam o valor de um numero x elevado a 2 e 3 respectivamente,
auxiliando no calculo das seguintes funçoes matematicas, que terao os parametros x, y e z digitados
pelo usuario:-}

--letra a
--f1(x) = x ^ 2 + 3x - 5x
f1(x) = quadrado(x) + (3 * x) - (5 * x)
{-  EXEMPLO:
    ENTRADA: f1(5)
    RETORNO: 15
-}

--letra b
--f2(x,y) = 2x^3 - 3xy + 10y
f2(x,y) = (2 * cubo(x)) - (3 * x * y) + (10 * y)
{-  EXEMPLO:
    ENTRADA: f2(2,8)
    RETORNO: 48
-}

--letra c
--f3(x,y,z) = 3xy + 2xz - 3yz
f3(x,y,z) = (3 * x * y) + (2 * x * z) - (3 * y * z)
{-  EXEMPLO:
    ENTRADA: f3(4,6,3)
    RETORNO: 42
-}

--letra d
--f4(x,y,z) = 4x^2y^3 - 2y^2z + 3yz^2
f4(x, y, z) = (4 * quadrado(x) * cubo(y)) - (2 * quadrado(y) * z) + (3 * y * quadrado(z))
{-  EXEMPLO:
    ENTRADA: f4(2,4,1)
    RETORNO: 1004
-}

--letra e
--f5(x,y) = (x+y)^2 / 2 * (x+y)
f5(x,y) = quadrado(x+y) / 2 * (x + y)
{-  EXEMPLO:
    ENTRADA: f5(6,8)
    RETORNO: 1372.0
-}


--Questão 03
--letra a 
{-  a soma dos angulos é igual a 180:
        2x + 5 + x + 10 = 180
    entao x é obtido pela seguinte equação:
        3x+ 15 =  180
        3x = 180 - 15
        x = (180 - 15)/3
    ENTRADA: valorX 
    RETORNO: 55.0
-} 
valorX = (180 - 15)/3


--letra b
{- Os angulos são obtidos utilizando a função valorX da letra a nas seguintes formulas:
    a1 = 2x + 5
    a2 = x + 10
-}

angulo1 = (2 * valorX) + 5
--ENTRADA: angulo1
--RETORNO: 115.0

angulo2 = valorX + 10
--ENTRADA: angulo2
--RETORNO: 65.0

{-  Portanto: 
    2x + 5 + x + 10 = 180 
    x = 55
    (2*55) + 5 + 55 +10 = 180
    115+65 = 180
    180 = 180
-}