-- TIPOS RECURSIVOS SIMPLES
-- CAMINO AL TESORO

data Objeto = Cacharro | Tesoro
    deriving (Show)
    
data Camino =   Fin | 
                Cofre [Objeto] Camino | 
                Nada Camino
    deriving (Show)

-------------------------------------------------------------
camino0 =  
   Cofre [Cacharro] $
   Fin

camino1 =
    Cofre [Cacharro] $
      Cofre [Cacharro] $
        Nada $
          Cofre [Tesoro, Tesoro, Tesoro] $
            Cofre [Cacharro] Fin

muchosCofresHayTesoro = 
  Cofre [Cacharro]  $  -- 0
  Cofre [Cacharro]  $  -- 1
  Cofre [Cacharro, Tesoro, Cacharro]  $ -- 2
  Cofre [Cacharro, Cacharro, Tesoro, Tesoro]  $ -- 3
  Cofre [Cacharro, Tesoro, Tesoro, Tesoro, Tesoro]  $ -- 4
  Cofre [Cacharro, Tesoro, Tesoro, Tesoro, Tesoro, Tesoro] -- 5
  Fin -- 6

sinCofres = 
  Nada $
    Nada $
     Nada Fin

muchosCofresSinTesoro = 
  Cofre [Cacharro] $
    Cofre [Cacharro] $
      Cofre [Cacharro] Fin

unCofre = [Cacharro, Cacharro]

-------------------------------------------------------------
hayTesoro :: Camino -> Bool
hayTesoro Fin = False 
hayTesoro (Nada restoCamino) = hayTesoro restoCamino
hayTesoro (Cofre obj restoCamino) = hayTesoroEntreObjetos obj || hayTesoro restoCamino

hayTesoroEntreObjetos :: [Objeto] -> Bool 
hayTesoroEntreObjetos [] = False 
hayTesoroEntreObjetos (o:os) = esTesoro o || hayTesoroEntreObjetos os 
                                    
esTesoro :: Objeto -> Bool 
esTesoro Cacharro = False 
esTesoro Tesoro = True

-------------------------------------------------------------
pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin = 0
pasosHastaTesoro (Nada restoCamino) = 1 + pasosHastaTesoro restoCamino
pasosHastaTesoro (Cofre obj restoCamino) =  if hayTesoroEntreObjetos obj 
                                                then 0
                                                else 1 + pasosHastaTesoro restoCamino

-------------------------------------------------------------
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn 0 camino = hayCofreEnEstePunto camino 
hayTesoroEn n Fin = False 
hayCaminoEn n (Nada restoCamino) = hayTesoroEn (n-1) restoCamino 
hayCaminoEn n (Cofre obj restoCamino) = hayTesoroEn (n-1) restoCamino 

hayCofreEnEstePunto :: Camino -> Bool 
hayCofreEnEstePunto Fin = False 
hayCofreEnEstePunto (Nada restoCamino) = False 
hayCofreEnEstePunto (Cofre obj restoCamino) = hayTesoroEntreObjetos obj

-------------------------------------------------------------
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n camino = cantTesorosEn camino >= n 

cantTesorosEn :: Camino -> Int 
cantTesorosEn Fin = 0
cantTesorosEn (Nada restoCamino) = cantTesorosEn restoCamino
cantTesorosEn (Cofre obj restoCamino) = cantTesorosEntreObjetos obj + cantTesorosEn restoCamino

cantTesorosEntreObjetos :: [Objeto] -> Int 
cantTesorosEntreObjetos [] = 0 
cantTesorosEntreObjetos (o:os) =    if esTesoro o 
                                        then 1 + cantTesorosEntreObjetos os 
                                        else cantTesorosEntreObjetos os 

-------------------------------------------------------------
cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre 0 0 camino = cantTesorosEnPuntoActual camino
cantTesorosEntre 0 hasta camino = cantTesorosEnPuntoActual camino + cantTesorosEntre 0 (hasta-1) (siguientePuntoDelCamino camino)
cantTesorosEntre desde hasta camino = cantTesorosEntre (desde-1) (hasta-1) (siguientePuntoDelCamino camino)

cantTesorosEnPuntoActual :: Camino -> Int 
cantTesorosEn Fin = 0
cantTesorosEn (Nada restoCamino) = 0
cantTesorosEn (Cofre obj restoCamino) = cantTesorosEntreObjetos obj

siguientePuntoDelCamino :: Camino -> Camino 
siguientePuntoDelCamino Fin = error "Llegaste al final, no hay mas camino"
siguientePuntoDelCamino (Nada restoCamino) = restoCamino
siguientePuntoDelCamino (Cofre obj restoCamino) = restoCamino