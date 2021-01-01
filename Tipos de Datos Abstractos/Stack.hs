module Stack(
    Stack,
    emptyS,
    isEmptyS,
    push,
    top,
    pop,
    lenS
)where

data Stack a = ConsSt [a] 
    deriving (Show)

p1 :: Stack Int
p1 = ConsSt [5,4,3,2,1] 

---------------------------------------
emptyS :: Stack a
emptyS = ConsSt []
---------------------------------------

isEmptyS :: Stack a -> Bool
isEmptyS (ConsSt xs) = null xs

---------------------------------------
push :: a -> Stack a -> Stack a
push e (ConsSt xs) = ConsSt (e:xs)

---------------------------------------
top :: Stack a -> a
top (ConsSt []) = error "Stack vacio, no hay tope" 
top (ConsSt xs) = head xs

---------------------------------------
pop :: Stack a -> Stack a
pop (ConsSt []) = emptyS
pop (ConsSt xs) = ConsSt (tail xs) 

---------------------------------------
lenS :: Eq a => Stack a -> Int
lenS (ConsSt xs) = longitud xs 

longitud :: Eq a => [a] -> Int 
longitud [] = 0
longitud (x:xs) = 1 + longitud xs