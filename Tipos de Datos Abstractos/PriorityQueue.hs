module PriorityQueue(
    PriorityQueue,
    emptyPQ,
    isEmptyPQ,
    insertPQ,
    findMinPQ,
    deleteMinPQ
)where

data PriorityQueue a = ConsPQ [a]
    deriving (Show)

----------------------------------------------
pq1 :: PriorityQueue Int 
pq1 = ConsPQ [5,6,1,4,99,0]

----------------------------------------------
emptyPQ :: PriorityQueue a
emptyPQ = ConsPQ []

----------------------------------------------
isEmptyPQ :: PriorityQueue a -> Bool 
isEmptyPQ (ConsPQ xs) = null xs 

----------------------------------------------
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a 
insertPQ e (ConsPQ xs) = ConsPQ (e:xs)

----------------------------------------------
findMinPQ :: Ord a => PriorityQueue a -> a 
findMinPQ (ConsPQ xs) = minimum xs

----------------------------------------------
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a 
deleteMinPQ (ConsPQ xs) = ConsPQ (deleteMinL xs)

deleteMinL :: Ord a => [a] -> [a]
deleteMinL [] = []
deleteMinL (x:xs) = if x == minimum (x:xs)
                        then xs 
                        else x : deleteMinL xs





