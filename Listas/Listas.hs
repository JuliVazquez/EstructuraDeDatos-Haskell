l1 :: [Int]
l1 = 1:2:3:4:5:6:[]

l2 :: [Int]
l2 = 1:5:1:4:4:9:4:[]

lb1 :: [Bool]
lb1 = True : True : False : True : []

lb2 :: [Bool]
lb2 = True : True : True : True : []

--RECURSION SOBRE LISTAS 

sumatoria :: [Int] -> Int 
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

-----------------------------------------------
longitud :: [a] -> Int 
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

------------------------------------------------
sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (x:xs) = (x+1) : sucesores xs

------------------------------------------------
conjuncion :: [Bool] -> Bool
conjuncion [] = True 
conjuncion (x:xs) = x && conjuncion xs

------------------------------------------------
disyuncion :: [Bool] -> Bool
disyuncion [] = False 
disyuncion (x:xs) = x || disyuncion xs

------------------------------------------------
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs

------------------------------------------------
pertenece :: Eq a => a -> [a] -> Bool 
pertenece e [] = False
pertenece e (x:xs) = (e == x) || pertenece e xs

------------------------------------------------
apariciones :: Eq a => a -> [a] -> Int
apariciones e [] = 0
apariciones e (x:xs) =  if e == x 
                            then 1 + apariciones e xs 
                            else apariciones e xs 

------------------------------------------------
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA e [] = []
losMenoresA e (x:xs) =  if x < e 
                            then x : losMenoresA e xs 
                            else losMenoresA e xs

------------------------------------------------
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]            -- lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n [] = []
lasDeLongitudMayorA n (x:xs) =  if(longitud x > n)
                                    then x : lasDeLongitudMayorA n xs
                                    else lasDeLongitudMayorA n xs


------------------------------------------------
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e = [e]
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e

------------------------------------------------
concatenar :: [a] -> [a] -> [a]
concatenar xs [] = xs 
concatenar [] ys = ys 
concatenar (x:xs) (ys) = x : concatenar xs ys

------------------------------------------------
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

------------------------------------------------
elMinimo :: Ord a => [a] -> a
elMinimo [] = error "No hay minimo en una lista vacia"
elMinimo (x:[]) = x 
elMinimo (x:xs) = min x (elMinimo xs)
