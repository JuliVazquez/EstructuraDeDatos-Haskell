
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]

data Barril = Comida | Oxigeno | Torpedo | Combustible

data SectorId = String 

data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)

--Esta representación utiliza:
--Un Map que relaciona para cada SectorId su sector correspondiente.
--Otro Map que relaciona para cada Nombre de tripulante el tripulante con dicho nombre.
--Una MaxHeap que incluye a todos los tripulantes de la nave, cuyo criterio de ordenado es por rango de los tripulantes.

----------------------------------------------------------------------
--Anexo de interfaces

--Sector, --siendo C la cantidad de contenedres y T la cantidad de tripulantes:
crearS :: SectorId -> Sector --O(1)
sectorId :: Sector -> SectorId --O(1)
componentesS :: Sector -> [Componente] --O(1)
tripulantesS :: Sector -> Set Nombre --O(1)
agregarC :: Componente -> Sector -> Sector --O(1)
agregarT :: Nombre -> Sector -> Sector --O(log T)
----------------------------------------------------------------------

--Tripulante, siendo S la cantidad de sectores:
crearT :: Nombre -> Rango -> Tripulante --O(1)
asignarS :: SectorId -> Tripulante -> Tripulante --O(log S)
sectoresT :: Tripulante -> Set SectorId -- O(1)
nombre :: Tripulante -> String --O(1)
rango :: Tripulante -> Rango --O(1)

----------------------------------------------------------------------
--Set, siendo N la cantidad de elementos del conjunto:
emptyS :: Set a --O(1)
addS :: a -> Set a -> Set a --O(log N)
belongsS :: a -> Set a -> Bool --O(log N)
unionS :: Set a -> Set a -> Set a --O(N log N)
setToList :: Set a -> [a] --O(N)
sizeS :: Set a -> Int -- O(1)

----------------------------------------------------------------------
--MaxHeap, siendo M la cantidad de elementos en la heap:
emptyH :: MaxHeap a --O(1)
isEmptyH :: MaxHeap a -> Bool --O(1)
insertH :: a -> MaxHeap a -> MaxHeap a --O(log M)
maxH :: MaxHeap a -> a --O(1)
deleteMaxH :: MaxHeap a -> MaxHeap a --O(log M)

----------------------------------------------------------------------
--Map, siendo K la cantidad de claves distintas en el map:
emptyM :: Map k v --O(1)
assocM :: k -> v -> Map k v -> Map k v --O(log K)
lookupM :: k -> Map k v -> Maybe v --O(log K)
deleteM :: k -> Map k v -> Map k v --O(log K)
domM :: Map k v -> [k] --O(K)

----------------------------------------------------------------------
--IMPLEMENTACION DE LA INTERFAZ DE NAVE

construir :: [SectorId] -> Nave
construir xs = N (agregarSectoresVaciosM xs emptyM) emptyM emptyH

agregarSectoresVaciosM :: [SectorId] -> Map SectorId Sector -> Map SectorId Sector
agregarSectoresVaciosM [] map = map 
agregarComponentesMap (x:xs) map = assocM x (crearS x) (agregarSectoresVaciosM xs map)

----------------------------------------------------------------------
ingresarT :: Nombre -> Rango -> Nave -> Nave
ingresarT nomNuevo ranNuevo (N mapSectores mapTrips mhTrips) = 
    N mapSectores (assocM nomNuevo (crearT nomNuevo ranNuevo) mapTrips) (insertH (crearT nomNuevo ranNuevo))

----------------------------------------------------------------------
sectoresAsignados :: Nombre -> Nave -> Set SectorId
sectoresAsignados nomTrip (N mapSec mapTrips mhTrips) = sectoresT(fromJust(lookupM nomTrip mapTrip))  

fromJust :: Maybe a -> a 
fromJust (Just x) -> x

----------------------------------------------------------------------
 datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
 datosDeSector idBuscado (N mapSec mapTrips mhTrips) = 
     ( tripulantesS(fromJust(lookupM idBuscado mapSec)) , componentesS(fromJust(lookupM idBuscado mapSec) )

----------------------------------------------------------------------
tripulantesN :: Nave -> [Tripulante]
tripulantesN (N mapSec mapTrips mhTrips) = toOrderedList mhTrips

toOrderedList :: MaxHeap Tripulante -> [Tripulante]
toOrderedList mhTrips = if isEmptyH mhTrips 
                            then []
                            else maxH mhTrips : toOrderedList(deleteMaxH mhTrips)

----------------------------------------------------------------------
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector comps sId (N mapSec mapTrips mhTrips) = N (actualizarComponentesEnMapSectores comps sId mapSec) mapTrips mhTrips

actualizarComponentesEnMapSectores :: [Componente] -> SectorId -> Map SectorId Sector -> Map SectorId Sector
actualizarComponentesEnMapSectores comps sId map = 
    case lookupM sId map of 
        Just sector -> assocM sId (sectorConComponentesAgregados comps sector) map
        Nothing     -> map      -- si no encontro el sector, devuelve el map de sectores tal cual estaba
    
sectorConComponentesAgregados :: [Componente] -> Sector -> Sector 
sectorConComponentesAgregados [] sector = sector 
sectorConComponentesAgregados (c:cs) sector = agregarC c (sectorConComponentesAgregados cs sector)

----------------------------------------------------------------------
asignarASector :: Nombre -> SectorId -> Nave -> Nave
asignarASector nomTrip sId (N mapSec mapTrips mhTrips) = 
    N (agregarTripASectorMap nomTrip sId mapSec) (agregarSectorATripEnMap nomTrip sId mapTrips) (agregarSectorATripEnHeap nomTrip sId mhTrips)

agregarTripASectorMap :: Nombre -> SectorId -> Map SectorId Sector -> Map SectorId Sector 
agregarTripASectorMap nomTrip sId mapSector = assocM sId (agregarT nomTrip (fromJust(lookupM sId mapSector))) mapSector

agregarSectorATripEnMap :: Nombre -> SectorId -> Map Nombre Tripulante -> Map Nombre Tripulante
agregarSectorATripEnMap nomTrip sId mapTrip = assocM nomTrip (asignarS sId (fromJust(lookupM nomTrip mapTrip))) mapTrip

agregarSectorATripEnHeap :: Nombre -> SectorId -> MaxHeap Tripulante -> MaxHeap Tripulante
agregarSectorATripEnHeap nomTrip sId heapTrip = 
    if nombre(maxH heapTrip) == nomTrip 
        then insertH (asignarS sId (maxH heapTrip)) (deleteMaxH heapTrip)
        else insertH (maxH heapTrip) (agregarSectorATripEnHeap nomTrip sId (deleteMaxH heapTrip))

--------------------------------------------------------------------------------------------------------------
--IMPLEMENTACIONES COMO USUARIO DE NAVE

sectores :: Nave -> Set SectorId
sectores nave = sectoresAsignadosATripulacion (tripulantesN nave) nave 

sectoresAsignadosATripulacion :: [Tripulante] -> Nave -> Set SectorId 
sectoresAsignadosATripulacion [] nave = emptyS 
sectoresAsignadosATripulacion (t:ts) nave = unionS (sectoresAsignados (nombre t) nave) (sectoresAsignadosATripulacion ts nave)

----------------------------------------------------------------------
sinSectoresAsignados :: Nave -> [Tripulante]
sinSectoresAsignados nave = losQueNoTienenSectorAsignardo (tripulantesN nave) nave 

losQueNoTienenSectorAsignardo :: [Tripulante] -> Nave -> [Tripulante]
losQueNoTienenSectorAsignardo [] nave = []
losQueNoTienenSectorAsignardo (t:ts) nave = if sectoresAsignados (nombre t) nave == emptyS 
                                                then t : losQueNoTienenSectorAsignardo ts nave 
                                                else losQueNoTienenSectorAsignardo ts nave 

----------------------------------------------------------------------
barriles :: Nave -> [Barril]
barriles nave = barrilesDeSectores (setToList(sectores nave)) nave 

barrilesDeSectores :: [SectorId] -> Nave -> [Barril]
barrilesDeSectores [] nave = []
barrilesDeSectores (s:ss) nave = (barrilesDelSector s nave) ++ barrilesDeSectores ss nave

barrilesDelSector :: Sector -> Nave -> [Barril]
barrilesDelSector sector nave = snd(datosDeSector sectorId(sector) nave)

-- NO PUEDO ACCEDER SOLO A LOS BARRILES COMO USUARIO, NO DEBERIA CONOCER LA ESTRUCTURA DE UN SECTOR, Y NO TENGO FUNCION QUE ME RETORNE
--BARRILES, SOLO COMPONENTES, POR LO TANTO, SOLO PUEDO DEVOLVER UNA LISTA DE COMPONENTES
