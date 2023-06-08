module Lib where

sumar10 :: [Int] -> [Int]
sumar10 [] = []
sumar10 (y:ys) = (y + 10) : sumar10 ys

multiplicarPor2 :: [Int] -> [Int]
multiplicarPor2 [] = []
multiplicarPor2 (cabeza:cola) = (cabeza * 2) : multiplicarPor2 cola

-- En Haskell esta funcion ya existe y se llama map
hacerOperacion :: (t -> a) -> [t] -> [a]
hacerOperacion operacion [] = []
hacerOperacion operacion (x:xs) = (operacion x) : hacerOperacion operacion xs

sumar10_2 lista = hacerOperacion (+10) lista 
multiplicarPor2_version2 x = hacerOperacion (*2) x



filtrarMod2 [] = []
filtrarMod2 (x:xs)
    | even x = x : filtrarMod2 xs
    | otherwise = filtrarMod2 xs

-- En Haskell esta funcion ya existe y se llama filter
filtrar :: (a -> Bool) -> [a] -> [a]
filtrar _ [] = []
filtrar condicion (x:xs)
    | condicion x = x : filtrar condicion xs
    | otherwise = filtrar condicion xs

esVocal 'a' = True
esVocal 'e' = True
esVocal _ = False

data Rectangulo = Rectangulo { base :: Float, altura :: Float } deriving Show

listaDeRectangulos :: [Rectangulo]
listaDeRectangulos = [ Rectangulo 10 2, Rectangulo 4 4 ]

areaDeRectangulo :: Float -> Float -> Float
areaDeRectangulo base altura = base * altura


modTupla (x, y) = mod x y

-- otros temas vistos en clase:
-- all
-- any
-- Composicion de funciones