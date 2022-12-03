module Atividade08 where
{- Para executar o programa basta charmar a funcao main, com duas listas como parametro: main([],[]).-}

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

--Menu principal
main(p,m) = do
                putStrLn "1- Cadastrar Pessoa"
                putStrLn "2- Cadastrar Medidas"
                putStrLn "3- Imprimir Pessoa"
                putStrLn "4- Imprimir Medidas"
                putStrLn "5- Qnt de mulheres cadastradas"
                putStrLn "6- Qnt de pessoas menores que uma idade"
                putStrLn "7- Pessoas com mais de 50 anos que possuem mais de 1.65m de altura e são do sexo feminino"
                putStrLn "8- Pessoas que que tem mais do que uma dada idade e menor do que uma dada altura."
                putStrLn "9- Sair"
                putStr "-> "
                x <- getLine
                menu(read x :: Int,p,m)

menu(x,p,m)
 |x == 1 = do
            pe <- lerPessoa
            p2 <- addPessoa(p,[pe])
            main(p2,m)
 |x == 2 = do
            med <- lerMedidas(p)
            m2 <- addMedidas(m,[med])
            main(p,m2)
 |x == 3 = do
            cpf <- lerCpf
            imprimir_pessoa(p,cpf)
            main(p,m)
 |x == 4 = do
            cpf <- lerCpf
            imprimir_medidas(m,cpf)
            main(p,m)
 |x == 5 = do
            putStr "Qnt de mulheres = " >> putStrLn(show(cont_mulheres(p,"feminino")))
            main(p,m)
 |x == 6 = do
            putStr "Digite a idade: "
            id <- getLine
            putStr "Qnt = " >> putStrLn(show(cont_idade(p,read id :: Integer)))
            main(p,m)
 |x == 7 = do
            putStr "Pessoas com mais de 50 anos que possuem mais de 1.65m de altura e são do sexo feminino"
            putStrLn(show(cont_altura(p,m)))
            main(p,m)
 |x == 8 = do
            putStr "Digite a idade: "
            id <- getLine
            al <- lerAltura
            putStr "Pessoas que tem mais de " >> putStr(show(id)) >> putStr "e"
            putStr "e sao menores que " >> putStrLn(show(al))
            putStrLn(show(contIdadeAltura(p,m,read id :: Integer,al)))
            main(p,m)
 |x == 9 = putStrLn "Programa Encerrado!"


--ler tipos
lerCpf :: IO CPF
lerCpf = do
           putStr "Digite o CPF: "
           cpf <- getLine
           if (length cpf == 11)
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
            if ((s == "feminino")||(s == "masculino")||(s == "outro"))
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

verifica_cpf :: ([Pessoa],CPF) -> Bool
verifica_cpf([],cpf) = False
verifica_cpf((c,n,d,s):r, cpf)
 |c == cpf = True
 |otherwise = verifica_cpf(r,cpf)


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
                                   

addPessoa :: ([Pessoa], [Pessoa]) -> IO [Pessoa]
addPessoa(lp,p) = return (lp++p)


addMedidas :: ([Medidas], [Medidas]) -> IO [Medidas]
addMedidas(lm,m) = return (lm++m)
                    
--FUNCOES 
--letra a
imprimir_pessoa([],cpf) = putStrLn "CPF nao encontrado"
imprimir_pessoa((c,n,d,s):r,cpf)
 |c == cpf = do
                putStr "CPF " >> putStrLn(show(c))
                putStr "Nome " >> putStrLn(show(n))
                putStr "Data de nascimento " >> putStrLn(show(d))
                putStr "Sexo " >> putStrLn(show(s))
 |otherwise = imprimir_pessoa(r,cpf)

--letra b
imprimir_medidas :: ([Medidas],CPF) -> IO()
imprimir_medidas([],cpf) = putStrLn "CPF nao encontrado"
imprimir_medidas((c,p,a):r,cpf)
 |c == cpf = do
                putStr "CPF " >> putStrLn(show((c)))
                putStr "Peso " >> putStrLn(show((p)))
                putStr "Altura " >> putStrLn(show((a)))
 |otherwise = imprimir_medidas(r,cpf)

--letra c
cont_mulheres :: ([Pessoa],String) -> Int
cont_mulheres([],f) = 0
cont_mulheres((c,n,d,s):r,f)
 |s == f = 1 + cont_mulheres(r,f)
 |otherwise = cont_mulheres(r,f)

--letra d
cont_idade :: ([Pessoa],Integer) -> Int
cont_idade([],id) = 0
cont_idade((c,n,(d,m,a),s):r,id)
 |(2021 - a) == id = 1 + cont_idade(r,id)
 |otherwise = cont_idade(r,id)

--letra f
cont_altura :: ([Pessoa],[Medidas]) -> [Nome]
cont_altura([],[]) = []
cont_altura((c1,n,(d,m,a),s):r1, (c2,p,al):r2)
 |(2021 - a > 50) && (al > 1.65) && (s == "feminino") = n: cont_altura(r1,r2)
 |otherwise = cont_altura(r1,r2)

--letra g
contIdadeAltura :: ([Pessoa],[Medidas],Integer,Altura) -> [Nome]
contIdadeAltura([],[],id,alt) = []
contIdadeAltura((c1,n,(d,m,a),s):r1,(c2,p,al):r2,id,alt)
 |(2021 - a > id) && (al < alt) = n: contIdadeAltura(r1,r2,id,alt)
 |otherwise = contIdadeAltura(r1,r2,id,alt)