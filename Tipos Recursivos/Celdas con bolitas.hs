-- TIPOS RECURSIVOS SIMPLES
-- CELDAS CON BOLITAS

data Color = Azul | Rojo
    deriving (Show)

data Celda =    Bolita Color Celda | 
                CeldaVacia
    deriving (Show)

celda1 :: Celda 
celda1 = Bolita Rojo (Bolita Azul (Bolita Azul CeldaVacia))

celda2 :: Celda 
celda2 = CeldaVacia

------------------------------------------------------------------------
nroBolitas :: Color -> Celda -> Int
nroBolitas colBuscado CeldaVacia = 0 
nroBolitas colBuscado (Bolita c restoCelda) =   if sonMismoColor colBuscado c 
                                                    then 1 + nroBolitas colBuscado restoCelda 
                                                    else nroBolitas colBuscado restoCelda

sonMismoColor :: Color -> Color -> Bool 
sonMismoColor Azul Azul = True 
sonMismoColor Rojo Rojo = True 
sonMismoColor _ _ = False

------------------------------------------------------------------------
poner :: Color -> Celda -> Celda
poner cNuevo celda = Bolita cNuevo celda

------------------------------------------------------------------------
sacar :: Color -> Celda -> Celda
sacar cSacar CeldaVacia = CeldaVacia 
sacar cSacar (Bolita c restoCelda) =    if sonMismoColor cSacar c 
                                            then restoCelda 
                                            else Bolita c (sacar cSacar restoCelda)

------------------------------------------------------------------------
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 cNuevo celda = celda
ponerN n cNuevo celda = Bolita cNuevo (ponerN (n-1) cNuevo celda)

------------------------------------------------------------------------

