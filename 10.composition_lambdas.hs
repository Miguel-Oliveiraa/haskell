-- 10. Funções como valores: composição e lambdas
-- ex1
-- Dada uma função f do tipo t -> u -> v,
-- defina uma expressão da forma
-- (\... -> ...)

-- para uma função do tipo u -> t -> v que
-- se comporta como f mas recebe seus
-- argumentos na ordem inversa
f:: Double -> Double -> Double
f t u = t / u

g = (\x y -> y / x)

