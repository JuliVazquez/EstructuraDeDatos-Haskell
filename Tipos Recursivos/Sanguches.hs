--TIPOS RECURSIVOS
-- SANGUCHES

data Sanguche = Pan Relleno 
    deriving Show

data Relleno = 
  Feta TipoDeFeta Relleno | 
  Aire
  deriving Show
  
data TipoDeFeta = 
  Queso | 
  Jamon | 
  Mortadela | 
  Salame
  deriving Show

-----------------------------------------------------------------
s1 :: Sanguche
s1 = Pan (Aire) 

s2 :: Sanguche 
s2 = Pan (Feta Queso (Feta Salame Aire))

s3 :: Sanguche 
s3 = Pan (Feta Jamon (Feta Jamon Aire))

s4 :: Sanguche 
s4 = Pan (Feta Queso (Feta Jamon (Feta Queso (Feta Mortadela Aire))))

-----------------------------------------------------------------
rellenoDeAire :: Sanguche -> Bool
rellenoDeAire (Pan relleno) = esSoloAire relleno 

esSoloAire :: Relleno -> Bool 
esSoloAire Aire = True 
esSoloAire _ = False

-----------------------------------------------------------------
esTortitaDeJamon :: Sanguche -> Bool
esTortitaDeJamon (Pan relleno) = (not(esSoloAire relleno)) && soloHayJamon relleno 
    
soloHayJamon :: Relleno -> Bool 
soloHayJamon Aire = True 
soloHayJamon (Feta fiambre restoFetas) = esJamon fiambre && soloHayJamon restoFetas

esJamon :: TipoDeFeta -> Bool 
esJamon Jamon = True 
esJamon _ = False

-----------------------------------------------------------------
mandaleNDe :: Int -> TipoDeFeta -> Sanguche -> Sanguche
mandaleNDe n fiambre (Pan relleno) = Pan(agregarNFetasDe n fiambre relleno)

agregarNFetasDe :: Int -> TipoDeFeta -> Relleno -> Relleno 
agregarNFetasDe 0 fiambre relleno = relleno 
agregarNFetasDe n fiambre fetasAnteriores = Feta fiambre (agregarNFetasDe (n-1) fiambre fetasAnteriores)

-----------------------------------------------------------------
peroSinQueso :: Sanguche -> Sanguche
peroSinQueso (Pan relleno) = Pan(sacarQuesoDe relleno)

sacarQuesoDe :: Relleno -> Relleno 
sacarQuesoDe Aire = Aire 
sacarQuesoDe (Feta fiambre restoFetas) =    if esQueso fiambre 
                                                then sacarQuesoDe restoFetas
                                                else Feta fiambre (sacarQuesoDe restoFetas)

esQueso :: TipoDeFeta -> Bool 
esQueso Queso = True 
esQueso _ = False

-----------------------------------------------------------------
ordenadosPorCantidad :: Sanguche -> [(TipoDeFeta, Int)]
ordenadosPorCantidad (Pan relleno) = ordenarRellenoPorCantidad relleno 

ordenarRellenoPorCantidad :: Relleno -> [(TipoDeFeta, Int)]
ordenarRellenoPorCantidad Aire = []
ordenarRellenoPorCantidad (Feta fiambre restoFiambre) = sumarFeta fiambre (ordenarRellenoPorCantidad restoFiambre)

sumarFeta :: TipoDeFeta -> [(TipoDeFeta,Int)] -> [(TipoDeFeta,Int)]
sumarFeta fiambreASumar [] = [(fiambreASumar, 1)]
sumarFeta fiambreASumar (f:fs) =  if esMismoFiambre fiambreASumar (fst(f))
                                    then (fiambreASumar, (snd(f)+1)) : fs       --modifico el primero y agreso al resto de la lista 
                                    else f : sumarFeta fiambreASumar fs

esMismoFiambre :: TipoDeFeta -> TipoDeFeta -> Bool 
esMismoFiambre Jamon Jamon = True 
esMismoFiambre Queso Queso = True 
esMismoFiambre Mortadela Mortadela = True 
esMismoFiambre Salame Salame = True 
esMismoFiambre _ _ = False
