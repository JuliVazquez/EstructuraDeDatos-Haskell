-- TIPOS RECURSIVOS 
-- MAPA DE TESOROS CON BIFUCARCACIONES 

data Dir = Izq | Der 
    deriving (Show)

data Objeto = Tesoro | Chatarra 
    deriving (Show)

data Cofre = Cofre [Objeto]
    deriving (Show)

data Mapa = Fin Cofre 
            | Bifurcacion Cofre Mapa Mapa 
    deriving (Show)

---------------------------------------------------------------

mapa1 :: Mapa
mapa1 = Fin (Cofre [Chatarra])

mapa2 :: Mapa
mapa2 = 
  Bifurcacion (Cofre [Chatarra])                --    __C
              (Fin (Cofre [Tesoro]))            -- C |__
              (Fin (Cofre [Tesoro]))            --      C

mapa3 :: Mapa
mapa3 = Bifurcacion (Cofre [Chatarra])          
                    mapa1                       
                    mapa1                       

mapa4 :: Mapa
mapa4 = Bifurcacion (Cofre [Tesoro])
                    mapa3
                    mapa3
mapa5 :: Mapa
mapa5 = Bifurcacion (Cofre [Tesoro]) 
            (Bifurcacion (Cofre [Tesoro])      -- Izquieda   
                (Fin (Cofre [Tesoro]))                -- Izquieda
                (Fin (Cofre [Chatarra])))             -- Derecha       
            (Bifurcacion (Cofre [Tesoro])      -- Derecha
                (Fin (Cofre [Chatarra]))                -- Izquieda
                (Bifurcacion (Cofre [Tesoro])           -- Derecha
                    (Fin (Cofre [Chatarra]))                    -- Izquieda
                    (Fin (Cofre [Tesoro]))))                    -- Derecha

mapa6 :: Mapa
mapa6 = Bifurcacion (Cofre [Chatarra]) 
            (Bifurcacion (Cofre [Chatarra])          -- Izquieda   
                (Fin (Cofre [Chatarra]))                    -- Izquieda
                (Fin (Cofre [Chatarra])))                   -- Derecha       
            (Bifurcacion (Cofre [Chatarra])           -- Derecha
                (Fin (Cofre [Chatarra]))                    -- Izquieda
                (Fin (Cofre [Tesoro])))                   -- Derecha

---------------------------------------------------------------
hayTesoro :: Mapa -> Bool
hayTesoro (Fin cofre) = hayTesoroEnCofre cofre
hayTesoro (Bifurcacion c mi md) = hayTesoroEnCofre c || hayTesoro mi || hayTesoro md

hayTesoroEnCofre :: Cofre -> Bool 
hayTesoroEnCofre (Cofre objs) = hayTesoroEnObjetos objs 

hayTesoroEnObjetos :: [Objeto] -> Bool 
hayTesoroEnObjetos [] = False 
hayTesoroEnObjetos (o:os) = esTesoro o || hayTesoroEnObjetos os 

esTesoro :: Objeto -> Bool 
esTesoro Tesoro = True 
esTesoro _ = False

------------------------------------------------------------------------
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] mapa = hayTesoroEnEstePunto mapa 
hayTesoroEn (d:ds) (Fin cofre) = error "No hay mas camino"
hayTesoroEn (d:ds) (Bifurcacion cofre mi md) =
    case d of 
        Izq -> hayTesoroEn ds mi 
        Der -> hayTesoroEn ds md

hayTesoroEnEstePunto :: Mapa -> Bool
hayTesoroEnEstePunto (Fin c) = hayTesoroEnCofre c 
hayTesoroEnEstePunto (Bifurcacion c _ _) = hayTesoroEnCofre c

------------------------------------------------------------------------
--Precondicion: existe un tesoro y es unico
caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin cofre) = []
caminoAlTesoro (Bifurcacion cofre mi md) =  if hayTesoroEnCofre cofre 
                                                then []
                                                else elegirCaminoAlTesoro (Bifurcacion cofre mi md)

elegirCaminoAlTesoro :: Mapa -> [Dir]
elegirCaminoAlTesoro (Bifurcacion cofre mi md) = if hayTesoro mi
                                                    then Izq : (caminoAlTesoro mi) 
                                                    else Der : (caminoAlTesoro md)

------------------------------------------------------------------------
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin c) = []
caminoDeLaRamaMasLarga (Bifurcacion c mi md) = if heightT mi > heightT md 
                                                    then Izq : caminoDeLaRamaMasLarga mi 
                                                    else Der : caminoDeLaRamaMasLarga md

heightT :: Mapa -> Int
heightT (Fin _) = 0 
heightT (Bifurcacion n ti td) = 1 + max (heightT ti) (heightT td)

------------------------------------------------------------------------
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin c) = [tesorosEn c] 
tesorosPorNivel (Bifurcacion c mi md) = tesorosEn c : unirPorNivel (tesorosPorNivel mi) (tesorosPorNivel md)

tesorosEn :: Cofre -> [Objeto]
tesorosEn (Cofre objs) = listTesorosEntreObjetos objs 

listTesorosEntreObjetos :: [Objeto] -> [Objeto]
listTesorosEntreObjetos [] = []
listTesorosEntreObjetos (o:os) = if esTesoro o
                                    then o : listTesorosEntreObjetos os 
                                    else listTesorosEntreObjetos os

unirPorNivel :: [[Objeto]] -> [[Objeto]] -> [[Objeto]]
unirPorNivel xss [] = xss 
unirPorNivel [] yss = yss 
unirPorNivel (xs:xss) (ys:yss) = (xs++ys) : unirPorNivel xss yss

------------------------------------------------------------------------
todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin c) = [[]]
todosLosCaminos (Bifurcacion c mi md) = (preppend Izq (todosLosCaminos mi)) ++ (preppend Der (todosLosCaminos md))

preppend :: Dir -> [[Dir]] -> [[Dir]]
preppend d [] = []
preppend d (ds:dss) = (d:ds) : (preppend d dss)