module Queue(
    Queue,
    emptyQ,
    isEmptyQ,
    queue,
    firstQ,
    dequeue
)where

data Queue a = ConsQ [a]
    deriving (Show)

q1 :: Queue Int
q1 = ConsQ [1,2,3,4,5]

q2 :: Queue Int
q2 = ConsQ [6,7,8]

----------------------------------------------------
emptyQ :: Queue a
emptyQ = ConsQ []

----------------------------------------------------
isEmptyQ :: Queue a -> Bool
isEmptyQ (ConsQ xs) = null xs

----------------------------------------------------
queue :: Eq a => a -> Queue a -> Queue a
queue e (ConsQ xs) = ConsQ (agregarAlFinal e xs)

agregarAlFinal :: Eq a => a -> [a] -> [a]
agregarAlFinal e [] = e:[]
agregarAlFinal e (x:xs) = x : agregarAlFinal e xs

----------------------------------------------------
firstQ :: Queue a -> a
firstQ (ConsQ []) = error "Cola vacia"
firstQ (ConsQ xs) = head xs

----------------------------------------------------
dequeue :: Queue a -> Queue a
dequeue (ConsQ []) = emptyQ
dequeue (ConsQ (x:xs)) = ConsQ xs




