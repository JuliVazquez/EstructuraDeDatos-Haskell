

module MultiSet(
    MultiSet
    --emptyMS,
    --addMS,
    --ocurrencesMS,
    --unionMS,
    --intersectionMS,
    --multiSetToList 
)where


data MultiSet a = MST (Map a Int)
    deriving (Show)

m3 :: Map String Int
m3 = 
    assocM "Varian" 1 $
    assocM "Jaina" 3 $
    assocM "Lorthemar" 2 $
    assocM "Baine" 5 $
    emptyM

m4 :: Map String Int
m4 = 
    assocM "Pepe" 2 $
    assocM "Juli" 3 $
    emptyM

ms1 = MST m3
ms2 = MST m4

----------------------------------------------------------------------------------------------------------------------
emptyMS :: MultiSet a
emptyMS = MST emptyM

----------------------------------------------------------------------------------------------------------------------
addMS :: Ord a => a -> MultiSet a -> MultiSet a
addMS e (MST map) =
    case lookupM e map of
        Just v  ->  MST(assocM x (v+1) map) 
        Nothing ->  MST(assocM e 1 map)

----------------------------------------------------------------------------------------------------------------------
ocurrencesMS :: Ord a => a -> MultiSet a -> Int
ocurrencesMS e (MST map) = 
    case lookupM e map of 
        Just v  -> v 
        Nothing -> 0

----------------------------------------------------------------------------------------------------------------------
multiSetToList :: MultiSet a -> [(a, Int)]
multiSetToList (MST map) = mapToList (keys map) map 

mapToList :: [Int] -> Map a Int -> [(a,Int)]
mapToList [] map = []
mapToList (k:ks) map = 
    case lookupM k map of
        Just v  -> (k,v) : mapToList ks map
        Nothing -> mapToList ks map
