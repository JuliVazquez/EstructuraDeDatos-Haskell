module Set(
    Set,
    emptyS,
    addS,
    belongs,
    sizeS,
    removeS,
    unionS,
    setToList
)where

data Set a = ConsS [a]
    deriving (Show)

s1 :: Set Int
s1 = ConsS [1,2,3,4,5,6]

s2 :: Set Int
s2 = ConsS [7,8,9]

---------------------------------------------------------------
emptyS :: Set a
emptyS = ConsS []

addS :: Eq a => a -> Set a -> Set a
addS e (ConsS xs) =  if elem e xs
                        then ConsS xs 
                        else ConsS (e:xs)

---------------------------------------------------------------
belongs :: Eq a => a -> Set a -> Bool
belongs e (ConsS xs) = elem e xs

---------------------------------------------------------------
sizeS :: Eq a => Set a -> Int
sizeS (ConsS xs) = longitud xs 

longitud :: Eq a => [a] -> Int 
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

---------------------------------------------------------------
removeS :: Eq a => a -> Set a -> Set a
removeS e (ConsS xs) = ConsS (removeL e xs) 

removeL :: Eq a => a -> [a] -> [a]
removeL e [] = []
removeL e (x:xs) =  if e == x 
                        then xs 
                        else x : (removeL e xs)

---------------------------------------------------------------
unionS :: Eq a => Set a -> Set a -> Set a
unionS (ConsS xs) (ConsS ys) = ConsS (unionList xs ys)

unionList :: Eq a => [a] -> [a] -> [a]
unionList [] ys = ys 
unionList xs [] = xs 
unionList (x:xs) ys =   if elem x ys 
                            then unionList xs ys 
                            else x : unionList xs ys 

---------------------------------------------------------------
setToList :: Eq a => Set a -> [a]
setToList (ConsS xs) = xs

