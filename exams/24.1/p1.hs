
-- 24.1
-- Cria uma cifra de cesar
type Chave = [(Char,Char)]
letras :: [Char]
letras = ['A'..'Z']
-- exemplo: cria_chave 3 ⇒ [('A','D'),('B','E'),('C','F'),...,('Z','C')]
criaChave :: Int -> Chave
criaChave n = zip letras (drop n (cycle letras))

-- Escreva a funcao crypt
crypt:: Chave -> String -> String
-- exemplo: crypt (cria_chave 3) "A LIGEIRA RAPOSA" ⇒ "D OLJHLUD UDSRVD"
crypt chav string = map (transforma chav) string

transforma [] letra = letra
transforma ((ch1, ch2):chs) letra | letra == ch1 = ch2
