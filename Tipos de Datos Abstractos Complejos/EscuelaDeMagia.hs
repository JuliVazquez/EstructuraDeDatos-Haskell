--Anexo de interfaces

--Mago, siendo H la cantidad de hechizos que sabe:
--crearM :: Nombre -> Mago 
--nombre :: Mago -> Nombre 
--aprender :: Hechizo -> Mago -> 
--hechizos :: Mago -> Set Hechizo 
----------------------------------------------------------------
--Set, siendo N la cantidad de elementos del conjunto:
--emptyS :: Set a 
--addS :: Ord a => a -> Set a -> 
--belongsS :: Ord a => a -> Set a -> Bool 
--unionS :: Ord a => Set a -> Set a -> Set a 
--sizeS :: Set a -> Int 
----------------------------------------------------------------
--PriorityQueue, siendo M la cantidad de elementos en la estructura:
--emptyPQ :: PriorityQueue a 
--isEmptyPQ :: PriorityQueue a -> Bool 
--insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a 
--maxPQ :: PriorityQueue a -> a 
--deleteMaxPQ :: Ord a => PriorityQueue a -> PriorityQueue a 
----------------------------------------------------------------
--Map, siendo K la cantidad de claves distintas en el map:
--emptyM :: Map k v 
--assocM :: Ord k => k -> v -> Map k v -> Map k v
--lookupM :: Ord k => k -> Map k v -> Maybe v 
--deleteM :: Ord k => k -> Map k v -> Map k v
--domM :: Map k v -> [k] 
---------------------------------------------------------------------------------------------------------
data EscuelaDeMagia = EDM (Set Hechizo) (Map Nombre Mago) (PriorityQueue Mago)


--IMPLEMENTACION DE INTEFAZ DE ESCUELA DE MAGIA
--------------------------------------------------------------
fundarEscuela :: EscuelaDeMagia
fundarEscuela = EDM (emptyS emptyM emptyPQ)

-------------------------------------------------------------- 
estaVacia :: EscuelaDeMagia -> Bool
estaVacia (EDM spells mapMagos pqMagos) = isEmptyPQ pqMagos

--------------------------------------------------------------
registrar :: Nombre -> EscuelaDeMagia -> EscuelaDeMagia
registrar nombreDelNuevo (EDM spells mapMagos pqMagos) = 
    EDM spells (assocM nombreDelNuevo (crearM nombreDelNuevo) mapMagos) (insertPQ (crearM nombreDelNuevo) pqMagos)

--------------------------------------------------------------
magos :: EscuelaDeMagia -> [Nombre]
magos (EDM spells mapMagos pqMagos) = domM mapMagos 

--------------------------------------------------------------
hechizosDe :: Nombre -> EscuelaDeMagia -> Set Hechizo
hechizosDe nombreMago (EDM spells mapMagos pqMagos) = hechizos(fromJust(lookupM nombreMago mapMagos))

fromJust :: Maybe a -> a 
fromJust (Just x) = x 

--------------------------------------------------------------
leFaltanAprender :: Nombre -> EscuelaDeMagia -> Int
leFaltanAprender name (EDM spells mapMagos pqMagos) = sizeS spells - sizeS(hechizos(fromJust(lookupM nombreMago mapMagos)))

--------------------------------------------------------------
egresarUno :: EscuelaDeMagia -> (Mago, EscuelaDeMagia)
egresarUno (EDM spells mapMagos pqMagos) = ( maxPq pqMagos , EDM spells (deleteFromMap (nombre(maxPq pqMagos)) mapMagos) (deleteMaxPQ pqMagos) )

--PREC: El mago existe
deleteFromMap :: Nombre -> Map Nombre Mago -> Map Nombre Mago 
deleteFromMap nombre mapMagos = deleteM nombre mapMagos

--------------------------------------------------------------
enseñar :: Hechizo -> Nombre -> EscuelaDeMagia -> EscuelaDeMagia
enseñar spell nombre (EDM setSpells mapMagos pqMagos) = 
    EDM setSpells (enseñarHechizoEnMap spell nombre mapMagos) (enseñarHechizoEnPq sepll nombre pqMagos)

enseñarHechizoEnMap :: Hechizo -> Nombre -> Map Nombre Mago -> Map Nombre Mago 
enseñarHechizoEnMap spell nombreM map = 
    case lookupM nombreM map of 
        Just mago   -> assocM nombreM (aprender spell mago) map 
        Nothing     -> error "No existe el mago"

enseñarHechizoEnPq :: Hechizo -> Nombre -> PriorityQueue Mago -> PriorityQueue Mago 
enseñarHechizoEnPq spell nombreM pqMagos =  if nombre(maxPQ pqMagos) == nombreM 
                                                then insertPQ (aprender spell(maxPQ pqMagos)) (deleteMaxPQ pqMagos)
                                                else insertPQ (maxPQ pqMagos) (enseñarHechizoEnPq spell nombreM (deleteMaxPQ pqMagos))

--------------------------------------------------------------
--IMPLEMENTACION COMO USUARIO DE ESCUELA DE MAGIA

hechizosAprendidos :: EscuelaDeMagia -> Set Hechizo
hechizosAprendidos edm = losHecizosAprendidosPorEstudiantes (magos edm) edm 

losHecizosAprendidosPorEstudiantes :: [Nombre] -> EscuelaDeMagia -> Set Hechizo 
losHecizosAprendidosPorEstudiantes [] edm = emptyS 
losHecizosAprendidosPorEstudiantes (n:ns) edm = unionS (hechizosDe n edm) (losHecizosAprendidosPorEstudiantes ns edm)

--------------------------------------------------------------
hayUnExperto :: EscuelaDeMagia -> Bool
hayUnExperto edm = leFaltanAprender (nombre(fst(egresarUno edm))) == 0

--------------------------------------------------------------
egresarExpertos :: EscuelaDeMagia -> ([Mago], EscuelaDeMagia)
egresarExpertos edm = if hayUnExperto edm 
                        then agregarExperto (fst(egresarUno edm) egresarExpertos(egresarUno edm)
                        else ([], edm)

agregarExperto :: Mago -> (Mago, EscuelaDeMagia) -> (Mago, EscuelaDeMagia)
agregarExperto mag (mags, edm) = (mag : mags, edm)