-- TIPOS RECURSIVOS 
-- NAVE ESPACIAL

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
  deriving (Show)

data Barril = Comida | Oxigeno | Torpedo | Combustible
  deriving (Show)

data Sector = S SectorId [Componente] [Tripulante]
  deriving (Show)

type SectorId = String

type Tripulante = String

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
  deriving (Show)

data Nave = N (Tree Sector)
  deriving (Show)

---------------------------------------------------------------------------------------------------------------------------------------------------------------
nave1 :: Nave
nave1 = N (t1)

t1 :: Tree Sector
t1 = NodeT sector1 
        (NodeT sector2 
          (NodeT sector3 
                  EmptyT
                  EmptyT)
          EmptyT)
        (NodeT sector4
          (NodeT sector5
              EmptyT
              EmptyT)
          EmptyT)

sector1 = S "F1" [LanzaTorpedos, Motor 25] ["Chewbacca","R2D2","Han Solo"]
sector2 = S "F2" [Almacen [Comida,Combustible]] ["C3P0"]
sector3 = S "F3" [Motor 15] []
sector4 = S "F4" [Almacen [Oxigeno,Torpedo],LanzaTorpedos] ["Leia"]
sector5 = S "F5" [Motor 5, LanzaTorpedos] ["Luke","R2D2"]

---------------------------------------------------------------------------------------------------------------------------------------------------
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

------------------------------------------------------------------------------------------------------------------------------------------
sectores :: Nave -> [SectorId]
sectores (N treeSector) = sectoresDe treeSector

sectoresDe :: Tree Sector -> [SectorId]
sectoresDe EmptyT = []
sectoresDe (NodeT sector si sd) = idSector sector : (sectoresDe si ++ sectoresDe sd) 

idSector :: Sector -> SectorId 
idSector (S id _ _) = id

----------------------------------------------------------------------------------------
poderDePropulsion :: Nave -> Int
poderDePropulsion (N treeSector) = propulsionAcumuladaEntreSectores treeSector 

propulsionAcumuladaEntreSectores :: Tree Sector -> Int 
propulsionAcumuladaEntreSectores EmptyT = 0 
propulsionAcumuladaEntreSectores (NodeT sector si sd) = poderDeMotorDeSector sector + propulsionAcumuladaEntreSectores si + propulsionAcumuladaEntreSectores sd

poderDeMotorDeSector :: Sector -> Int 
poderDeMotorDeSector (S _ componentes _) = poderDeMotorEntreComponentes componentes 

poderDeMotorEntreComponentes :: [Componente] -> Int 
poderDeMotorEntreComponentes [] = 0 
poderDeMotorEntreComponentes (c:cs) =   if esMotor c 
                                            then poderMotor c + poderDeMotorEntreComponentes cs 
                                            else poderDeMotorEntreComponentes cs 

esMotor :: Componente -> Bool 
esMotor (Motor _) = True 
esMotor _ = False

--Precondicion: el componente es motor
poderMotor :: Componente -> Int 
poderMotor (Motor potencia) = potencia

----------------------------------------------------------------------------------------
barriles :: Nave -> [Barril]
barriles (N treeSector) = listaDeBarriles treeSector 

listaDeBarriles :: Tree Sector -> [Barril] 
listaDeBarriles EmptyT = []
listaDeBarriles (NodeT sector si sd) = barrilesDeSector sector ++ listaDeBarriles si ++ listaDeBarriles sd

barrilesDeSector :: Sector -> [Barril]
barrilesDeSector (S _ componentes _) = barrilesDeLosComponentes componentes

barrilesDeLosComponentes :: [Componente] -> [Barril]
barrilesDeLosComponentes [] = []
barrilesDeLosComponentes (c:cs) =   if esAlmacen c 
                                        then barrilesDeAlmacen c ++ (barrilesDeLosComponentes cs)
                                        else barrilesDeLosComponentes cs

esAlmacen :: Componente -> Bool 
esAlmacen (Almacen xs) = True 
esAlmacen _ = False 

barrilesDeAlmacen :: Componente -> [Barril] 
barrilesDeAlmacen (Almacen barriles) = barriles

----------------------------------------------------------------------------------------
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector componentes sId (N treeSector) = N (buscarSectorYAgregarComponentes componentes sId treeSector)

buscarSectorYAgregarComponentes :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
buscarSectorYAgregarComponentes componentes sId EmptyT = EmptyT
buscarSectorYAgregarComponentes [] sId treeSector = treeSector
buscarSectorYAgregarComponentes cs sId (NodeT sector si sd) =   
     if sId == (idSector sector)
        then NodeT (agregarEnSectorActual sector cs) si sd
        else NodeT sector (buscarSectorYAgregarComponentes cs sId si) (buscarSectorYAgregarComponentes cs sId sd)

agregarEnSectorActual :: Sector -> [Componente] -> Sector 
agregarEnSectorActual (S id componentes trip) compsNuevos = (S id (componentes ++ compsNuevos) trip)

----------------------------------------------------------------------------------------
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA nTrip listIdS (N treeSector) = N (sectoresLuegoDeAsignacion nTrip listIdS treeSector)

sectoresLuegoDeAsignacion :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector 
sectoresLuegoDeAsignacion nTrip listIdS EmptyT = EmptyT
sectoresLuegoDeAsignacion nTrip [] treeSector = treeSector
sectoresLuegoDeAsignacion nTrip listIdS (NodeT sector si sd) = 
    if pertenece (idSector sector) listIdS
        then NodeT (asignarAlTripulanteEnSector nTrip sector) (sectoresLuegoDeAsignacion nTrip listIdS si) (sectoresLuegoDeAsignacion nTrip listIdS sd)
        else NodeT sector (sectoresLuegoDeAsignacion nTrip listIdS si) (sectoresLuegoDeAsignacion nTrip listIdS sd)

pertenece :: Eq a => a -> [a] -> Bool 
pertenece e [] = False 
pertenece e (x:xs) = e == x || pertenece e xs

asignarAlTripulanteEnSector :: Tripulante -> Sector -> Sector 
asignarAlTripulanteEnSector nTrip (S id comps tripulacion) = S id comps (nTrip : tripulacion)

----------------------------------------------------------------------------------------
sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados tripBuscado (N treeSector) = losSectoresDelTripulante tripBuscado treeSector

losSectoresDelTripulante :: Tripulante -> Tree Sector -> [SectorId]
losSectoresDelTripulante tripBuscado EmptyT = []
losSectoresDelTripulante tripBuscado (NodeT sector si sd) = 
    if tieneAsignacionEnSector tripBuscado sector 
        then (idSector sector) : (losSectoresDelTripulante tripBuscado si ++ losSectoresDelTripulante tripBuscado sd)
        else losSectoresDelTripulante tripBuscado si ++ losSectoresDelTripulante tripBuscado sd

tieneAsignacionEnSector :: Tripulante -> Sector -> Bool 
tieneAsignacionEnSector tripBuscado (S _ _ tripulantes) = pertenece tripBuscado tripulantes

----------------------------------------------------------------------------------------
tripulantes :: Nave -> [Tripulante]
tripulantes (N treeSector) = (listaDeTripulantes treeSector)

listaDeTripulantes :: Tree Sector -> [Tripulante]
listaDeTripulantes EmptyT = []
listaDeTripulantes (NodeT sector si sd) =  sinRepetidos(tripulantesDelSector sector ++ listaDeTripulantes si ++ listaDeTripulantes sd)

tripulantesDelSector :: Sector -> [Tripulante]
tripulantesDelSector (S _ _ tripulantes) = tripulantes 

sinRepetidos :: Eq a=> [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if pertenece x xs 
                        then sinRepetidos xs 
                        else x : sinRepetidos xs



