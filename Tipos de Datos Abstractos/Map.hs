module Map(
    Map
--    emptyM,
--    assocM,
--    lookupM,
--    deleteM,
--    keys
)where


{-INVARIANTE DE REPRESENTACION:
    -- No hay claves repetidas
-}
data Map k v = MKV [(k, v)]
    deriving (Show)

m1 = MKV [("KaelThas", 1000), ("Thrall", 2004), ("Atila", 2008)]
m2 = MKV [("Varian", 2016), ("Dante", 666)]

----------------------------------------------------------------------------------------
emptyM :: Map k v
emptyM = MKV []

----------------------------------------------------------------------------------------
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM nK nV (MKV map) = MKV (agregarAlMap nK nV map)

agregarAlMap :: Eq k => k -> v -> [(k,v)] -> [(k,v)]
agregarAlMap nK nV [] = [(nK,nV)]
agregarAlMap nK nV (m:ms) = if nK == fst(m)
                                then (nK, nV) : ms 
                                else m : agregarAlMap nK nV ms

----------------------------------------------------------------------------------------
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM key (MKV map) = findValue key map 

findValue :: Eq k => k -> [(k,v)] -> Maybe v
findValue key [] = Nothing 
findValue key (m:ms) =  if key == fst m
                            then Just (snd m)
                            else findValue key ms

----------------------------------------------------------------------------------------
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM key (MKV map) = MKV (deleteFromMap key map) 

deleteFromMap :: Eq k => k -> [(k,v)] -> [(k,v)]
deleteFromMap key [] = []
deleteFromMap key (m:ms) =  if key == fst m 
                                then ms 
                                else m : deleteFromMap key ms

----------------------------------------------------------------------------------------
keys :: Map k v -> [k]
keys (MKV map) = keysFromMap map 

keysFromMap :: [(k,v)] -> [k]
keysFromMap [] = []
keysFromMap (m:ms) = fst m : keysFromMap ms