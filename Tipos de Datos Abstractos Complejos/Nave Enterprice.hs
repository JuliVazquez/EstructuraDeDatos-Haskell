--IMPLEMENTACION DE LA INTERFAZ DEL TAD NAVE
data Nave = MkN (Map Sector (Set Tripulante)) (Heap Tripulante) (Sector, Int)

------------------------------------------------------------------------------
naveVacia :: [Sector] -> Nave
naveVacia listSectores = MkN (agregarSectoresAMap listSectores emptyM) emptyH (head listSectores, 0)

agregarSectoresAMap :: [Sector] -> Map Sector (Set Tripulante) -> Map Sector (Set Tripulante)
agregarSectoresAMap [] mapSector = mapSector
agregarSectoresAMap (s:ss) mapSector = assocM s emptyS (agregarSectoresAMap ss mapSector)

------------------------------------------------------------------------------
tripulantesDe :: Sector -> Nave -> Set Tripulante
tripulantesDe sector (MkN mapSec heapTrip maxSector) = 
    case lookupM sector mapSec of 
        Just setTrips   -> setTrips 
        Nothing         -> emptyS

------------------------------------------------------------------------------
sectores :: Nave -> [Sector]
sectores (MkN mapSec heapTrip maxSector) = domM mapSec

------------------------------------------------------------------------------
conMayorRango :: Nave -> Tripulante
conMayorRango (MkN mapSec heapTrip maxSector) =  findMin heapTrip

------------------------------------------------------------------------------
conMasTripulantes :: Nave -> Sector
conMasTripulantes (MkN mapSec heapTrip maxSector) = fst maxSector

------------------------------------------------------------------------------
conRango :: Rango -> Nave -> Set Tripulante
conRango rangoBuscado (MkN mapSec heapTrip maxSector) = tripulantesConRangoDe rangoBuscado heapTrip emptyS

tripulantesConRangoDe :: Rango -> Heap Tripulante -> Set Tripulante -> Set Tripulante
tripulantesConRangoDe rangoBuscado heapTrip setT = if rangoBuscado == rango(findMin heapTrip)
                                                        then addS (findMin heapTrip) (tripulantesConRangoDe rangoBuscado (deleteMin heapTrip) setT)
                                                        else tripulantesConRangoDe rangoBuscado (deleteMin heapTrip) setT

------------------------------------------------------------------------------
sectorDe :: Tripulante -> Nave -> Sector
sectorDe tripBuscado (MkN mapSec heapTrip maxSector) = findMin (buscarSectorParaTripulante tripBuscado (domM mapSec) mapSec emptyH) 

buscarTripulanteEntreSectores :: Tripulante -> [Sector] -> Map Sector (Set Tripulante) -> Heap Sector  -> Heap Sector
buscarTripulanteEntreSectores tripBuscado [] mapSector heapS = heapS
buscarTripulanteEntreSectores tripBuscado (s:ss) mapSector heapS = 
    case s mapSector of 
        Just setTrip ->  if belongs tripBuscado setTrip 
                            then insertH s heapS
                            else buscarTripulanteEntreSectores tripBuscado ss mapSector heapS 

        Nothing     -> buscarTripulanteEntreSectores tripBuscado ss mapSector heapS 

------------------------------------------------------------------------------
agregarTripulante :: Tripulante -> Sector -> Nave -> Nave
agregarTripulante nuevoTrip sector (MkN mapSec heapTrip maxSector) = 
    MkN (agregarTripASectorM nuevoTrip sector mapSec) heapTrip (verificarMaximoSector sector maxSector (agregarTripASectorM nuevoTrip sector mapSec))

agregarTripASectorM :: Tripulante -> Sector -> Map Sector (Set Tripulante) -> Map Sector (Set Tripulante)
agregarTripASectorM nuevoTrip sector mapSector = assocM sector (addS nuevoTrip (fromJust(lookupM sector mapSector))) mapSector

verificarMaximoSector :: Sector -> (Sector,Int) -> Map Sector (Set Tripulante) -> (Sector, Int)
verificarMaximoSector sectorConAgregado sectorMaximo mapS = 
    if (cantidadDeTripulantes sectorConAgredado mapS) > snd sectorMaximp
        then (sectorConAgregado, cantidadDeTripulantes sectorConAgredado mapS)
        else sectorMaximo

cantidadDeTripulantes :: Sector -> Map Sector (Set Tripulante) -> Int 
cantidadDeTripulantes sector mapSector = sizeS(fromJust(lookupM sector mapSector))
