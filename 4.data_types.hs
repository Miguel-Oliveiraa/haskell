-- tipos de dados
addEspacos:: Int -> String
addEspacos 0 = ""
addEspacos n = " " ++ addEspacos (n - 1)

paraDireita:: Int -> String -> String
paraDireita n string = string ++ addEspacos n

vendasPorSemana:: Int -> Int
vendasPorSemana 0 = 12
vendasPorSemana 1 = 14
vendasPorSemana 2 = 15

exibirVendas:: Int -> String
exibirVendas  n 
  | n == 0 =  "\n" ++ paraDireita 6 (show (0))  ++ show (vendasPorSemana (0))
  | n > 0 =  exibirVendas (n-1) ++ "\n" ++ paraDireita 6 (show (n)) ++ show (vendasPorSemana (n))

somaTotal:: Int -> Int
somaTotal n | n == 0 = vendasPorSemana 0
               | n > 0 = vendasPorSemana n + somaTotal (n-1)

imprimeTotal:: Int -> String
imprimeTotal n = paraDireita 2 "\nTotal" ++ show (somaTotal n)


imprimeMedia:: Int -> String
imprimeMedia n = paraDireita 2 "\nMedia" ++ show (toFloat(somaTotal(n)) / toFloat((n+1)))
  where toFloat = fromIntegral

imprimeTabela:: Int -> IO()
imprimeTabela n = putStrLn("Semana Venda" ++ exibirVendas n ++ imprimeTotal n ++ imprimeMedia n)
