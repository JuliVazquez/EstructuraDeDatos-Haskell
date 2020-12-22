-- TIPOS RECURSIVOS SIMPLES
-- ARBOLES BINARIOS

data Tree a =   EmptyT | 
                NodeT a (Tree a) (Tree a)
    deriving (Show)

--------------------------------------------------------------------    

-- Para printear arboles: 

pad :: [[Char]] -> [[Char]]
pad = zipWith (++) (repeat "   ")

arreglarT :: Show a => Tree a -> IO()
arreglarT EmptyT = putStrLn "-"

arreglarT tree = printRecursive (prettyprint_helper tree)
  where 
    printRecursive [] = putStrLn ""
    printRecursive (x:xs) = do
      putStrLn x
      printRecursive xs

prettyprint_helper (NodeT x left right)
  = (show x) : (prettyprint_subtree left right)
    where
      prettyprint_subtree left right =
        ((pad "+- " "|  ") (prettyprint_helper right))
          ++ ((pad "`- " "   ") (prettyprint_helper left))
      pad first rest = zipWith (++) (first : repeat rest)

prettyprint_helper EmptyT = [""]

--------------------------------------------------------------------------------------------------------------------------------------------
t1 :: Tree Int
t1 = NodeT 32
        (NodeT 35
          (NodeT 100
              (NodeT 15
                  EmptyT
                  EmptyT)
              EmptyT)
          EmptyT)
        (NodeT 40
          (NodeT 35
              EmptyT
              EmptyT)
          EmptyT)

t2 :: Tree Int 
t2 = NodeT 1 (NodeT 2 (NodeT 9 EmptyT EmptyT) (NodeT 3 EmptyT EmptyT)) (NodeT 5 EmptyT EmptyT)

----------------------------------------------------------------------------------------------------------------------------------
sumarT :: Tree Int -> Int
sumarT EmptyT = 0 
sumarT (NodeT n ti td) = n + sumarT ti + sumarT td

---------------------------------------------------------------------------------
sizeT :: Tree a -> Int
sizeT EmptyT = 0 
sizeT (NodeT n ti td) = 1 + sizeT ti + sizeT td

---------------------------------------------------------------------------------
mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT 
mapDobleT (NodeT n ti td) = NodeT (2*n) (mapDobleT ti) (mapDobleT td)

---------------------------------------------------------------------------------
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT e EmptyT = False 
perteneceT e (NodeT n ti td) = n == e || (perteneceT e ti) || (perteneceT e ti)

---------------------------------------------------------------------------------
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT e EmptyT = 0
aparicionesT e (NodeT n ti td) =    if e == n
                                        then 1 + (aparicionesT e ti) + (aparicionesT e td) 
                                        else (aparicionesT e ti) + (aparicionesT e td)

---------------------------------------------------------------------------------
leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT n td ti) =    if isEmptyT td && isEmptyT ti 
                                then n : (leaves td ++ leaves ti)
                                else (leaves td ++ leaves ti)

isEmptyT :: Tree a -> Bool 
isEmptyT EmptyT = True 
isEmptyT _ = False

---------------------------------------------------------------------------------
heightT :: Tree a -> Int
heightT EmptyT = 0 
heightT (NodeT n ti td) = 1 + max (heightT ti) (heightT td)

---------------------------------------------------------------------------------
mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT 
mirrorT (NodeT n ti td) = NodeT n (mirrorT td) (mirrorT ti)

---------------------------------------------------------------------------------
toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT n ti td) = n : (toList ti ++ toList td)

---------------------------------------------------------------------------------
levelN :: Int -> Tree a -> [a]
levelN lvl EmptyT = []
levelN 0 (NodeT n ti td) = [n]
levelN lvl (NodeT n ti td) = levelN (lvl-1) ti ++ levelN (lvl-1) td

---------------------------------------------------------------------------------
listPerLevel :: Tree a -> [[a]]
listPerLevel (EmptyT) = []
listPerLevel (NodeT n ti td) = [n] : unirPorNivel (listPerLevel ti) (listPerLevel td)

unirPorNivel :: [[a]] -> [[a]] -> [[a]]
unirPorNivel [] yss = yss
unirPorNivel xss [] = xss 
unirPorNivel (xs:xss) (ys:yss) = (xs++ys) : unirPorNivel xss yss
---------------------------------------------------------------------------------
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT n ti td) = if heightT ti >= heightT td   
                                    then n : ramaMasLarga ti 
                                    else n : ramaMasLarga td

---------------------------------------------------------------------------------
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT n EmptyT EmptyT) = [[n]]
todosLosCaminos (NodeT n ti td) = (preppend n (todosLosCaminos ti)) ++ (preppend n (todosLosCaminos td))

preppend :: a -> [[a]] -> [[a]]
preppend e [] = []
preppend e (ts:tss) = (e:ts) : (preppend e tss) 