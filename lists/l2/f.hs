main = do
    a <- getLine
    b <- getLine
    print (contagemNotas (read a) (read b))

contagemNotas :: [String] -> [String] -> Int
contagemNotas notasTiradas mediaFaculdade = length (filter (\x-> calcularMedia notasTiradas>=x) (converterNotas mediaFaculdade))
  where calcularMedia lista = media (converterNotas lista)


-- calcular media
media:: [Float] -> Float
media as = (foldr (+) 0 as)/fromIntegral (length as)

-- conventerNotas -> Numero
converterNotas:: [String] -> [Float]
converterNotas [] = []
converterNotas (a:as) = conversor a:converterNotas as

conversor:: String -> Float
conversor "A+" = 9.7
conversor "A" = 9.3
conversor "A-" = 9.0
conversor "B+" = 8.7
conversor "B" = 8.3
conversor "B-" = 8.0
conversor "C+" = 7.7
conversor "C" = 7.3
conversor "C-" = 7.0
conversor "D+" = 6.7
conversor "D" = 6.3
conversor "D-" = 6.0
conversor "F+" = 5.7
conversor "F" = 5.3
conversor "F-" = 5.0
