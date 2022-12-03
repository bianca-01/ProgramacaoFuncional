module Atividade8 where

--Tipos
type CPF = Integer
type Nome = String
type Sexo = String
type Dia = Integer
type Mes = Integer
type Ano = Integer
type DtNas = (Dia,Mes,Ano)
type Peso = Float
type Altura = Float
type Pessoa = (CPF,Nome, DtNas, Sexo)
type Medidas = (CPF, Peso, Altura)

--ler tipos
lerCpf :: IO CPF
lerCpf = do
           putStr "Digite o CPF: "
           cpf <- getLine
           if length cpf == 11
               then return (read cpf :: CPF)
               else do
                    putStrLn "CPF invalido, tente novamente"
                    lerCpf
lerNome :: IO Nome
lerNome = do
            putStr "Digite o nome: "
            n <- getLine
            return (read n :: Nome)

lerSexo :: IO Sexo
lerSexo = do
            putStr "Digite o sexo (feminino, masculino, outro): "
            s <- getLine
            if (s == "feminino")||(s == "masculino")||(s == "outro")
                then return (read s :: Sexo)
                else do
                       putStrLn "Opcao invalida, tente novamente"
                       lerSexo

lerPeso :: IO Peso
lerPeso = do
            putStr "Digite o peso: "
            p <- getLine
            return (read p :: Peso)

lerAltura :: IO Altura
lerAltura = do
              putStr "Digite a altura: "
              a <- getLine
              return (read a :: Altura)


lerData :: IO (Dia, Mes, Ano)
lerData = do
            putStrLn "Data de nascimento"
            putStr "Digite o dia: "
            d <- getLine
            putStr "Digite o mes: "
            m <- getLine
            putStr "Digite o ano: "
            a <- getLine
            if validarData(read d :: Dia, read m :: Mes, read a :: Ano)
                then return (read d :: Dia, read m :: Mes, read a :: Ano)
                else do
                     putStrLn "Data invalida, tente novamente"
                     lerData

validarData :: (Dia, Mes, Ano) -> Bool            
validarData(d,m,a)
 |m > 12 = False
 |d > 28 && m==2 = False
 |a > 2020 = False
 |(d == 31) && ((m == 4) || (m == 6) || (m == 9) || (m == 11)) = False
 |otherwise = True 


verifica_cpf :: Eq b1 => ([(b1, b2, c, d)], b1) -> Bool
verifica_cpf([],cpf) = False
verifica_cpf((c,n,d,s):r, cpf)
 |c == cpf = True
 |otherwise = verifica_cpf(r,cpf)


lerPessoa :: IO (CPF, Nome, (Dia, Mes, Ano), Sexo)
lerPessoa = do
              putStrLn "Digite os dados da pessoa:"
              cpf <- lerCpf
              n <- lerNome
              d <- lerData
              s <- lerSexo
              return (cpf, n, d, s)

lerMedidas :: [Pessoa] -> IO Medidas
lerMedidas((c,n,d,s):r) = do
                        putStrLn "Digite o cpf: "
                        cpf <- lerCpf
                        if verifica_cpf((c,n,d,s):r,cpf)
                            then do
                                    p <- lerPeso
                                    a <- lerAltura
                                    return (cpf, p, a)
                            else do
                                    putStrLn "CPF nao encontrado, tente novamente"
                                    lerMedidas((c,n,d,s):r)
                                   
--letra a
imprimir_pessoa :: (Eq b, Show b, Show a1, Show a2, Show a3) => ([(b, a1, a2, a3)], b) -> IO ()
imprimir_pessoa([],cpf) = putStrLn "CPF nao encontrado"
imprimir_pessoa((c,n,d,s):r,cpf)
 |c == cpf = do
                putStr "CPF " >> putStrLn(show(c))
                putStr "Nome " >> putStrLn(show(n))
                putStr "Data de nascimento " >> putStrLn(show(d))
                putStr "Sexo " >> putStrLn(show(s))
 |otherwise = imprimir_pessoa(r,cpf)

--letra b
imprimir_medidas :: (Eq b, Show b, Show a1, Show a2) => ([(b, a1, a2)], b) -> IO ()
imprimir_medidas([],cpf) = putStrLn "CPF nao encontrado"
imprimir_medidas((c,p,a):r,cpf)
 |c == cpf = do
                putStr "CPF " >> putStrLn(show((c)))
                putStr "Peso " >> putStrLn(show((p)))
                putStr "Altura " >> putStrLn(show((a)))
 |otherwise = imprimir_medidas(r,cpf)

--letra c
cont_mulheres :: (Num p, Eq b1) => ([(a, b2, c, b1)], b1) -> p
cont_mulheres([],f) = 0
cont_mulheres((c,n,d,s):r,f)
 |s == f = 1 + cont_mulheres(r,f)
 |otherwise = cont_mulheres(r,f)

--letra d
cont_idade :: (Eq b1, Num p, Num b1) => ([(a1, b2, (a2, b3, b1), d)], b1) -> p
cont_idade([],id) = 0
cont_idade((c,n,(d,m,a),s):r,id)
 |(2021 - a) == id = 1 + cont_idade(r,id)
 |otherwise = cont_idade(r,id)

--letra e
--letra f
cont_altura :: (Ord a1, Ord a2, Fractional a2, Num a1) => ([(a3, a4, (a5, b1, a1), [Char])], [(a6, b2, a2)]) -> [a4]
cont_altura([],[]) = []
cont_altura((c1,n,(d,m,a),s):r1, (c2,p,al):r2)
 |(2021 - a > 50) && (al > 1.65) && (s == "feminino") = n: cont_altura(r1,r2)
 |otherwise = cont_altura(r1,r2)

--letra g
contIdadeAltura :: (Num c, Ord c, Ord d1) => ([(a1, a2, (a3, b1, c), d2)], [(a4, b2, d1)], c, d1) -> [a2]
contIdadeAltura([],[],id,alt) = []
contIdadeAltura((c1,n,(d,m,a),s):r1,(c2,p,al):r2,id,alt)
 |(2021 - a > id) && (al < alt) = n: contIdadeAltura(r1,r2,id,alt)
 |otherwise = contIdadeAltura(r1,r2,id,alt)