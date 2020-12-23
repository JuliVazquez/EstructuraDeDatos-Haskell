--TIPOS RECURSIVOS 
-- LABERINTO

type Llave = Int      -- Las llaves son simples números.

data Dir = Izq | Der
  deriving (Show)

data Cofre = Cofre [Llave] Int
  deriving (Show)

data Laberinto = Salida
                | Celda Cofre
                | Bifurcacion Laberinto Laberinto
    deriving (Show)
-----------------------------------------------------------------------------------------------------------------------------------

lab1 :: Laberinto
lab1 = b1

b1 = Bifurcacion b2 c1
b2 = Bifurcacion s1 c2 
c1 = Celda cof1
c2 = Celda cof2
s1 = Salida
cof1 = Cofre [77] 55
cof2 = Cofre [77,15] 15

lab2 :: Laberinto
lab2 = bf1

bf1 = Bifurcacion bf2 bf3
bf2 = Bifurcacion bf4 bf5
bf3 = Bifurcacion ce1 sa2
bf4 = Bifurcacion ce2 sa1  
bf5 = Bifurcacion ce3 ce4  
ce1 = Celda cofr1
ce2 = Celda cofr2
ce3 = Celda cofr3
ce4 = Celda cofr4
sa1 = Salida
sa2 = Salida
cofr1 = Cofre [77] 55
cofr2 = Cofre [77,15] 65
cofr3 = Cofre [] 25
cofr4 = Cofre [141] 99

-----------------------------------------------------------------------------------------------------------------------------------
cantidadDeSalidas :: Laberinto -> Int
cantidadDeSalidas Salida = 1
cantidadDeSalidas (Celda c) = 0 
cantidadDeSalidad (Bifurcacion li ld) = cantidadDeSalidas li + cantidadDeSalidas ld

-----------------------------------------------------------------------------------------
queLlavesDeboTener :: Laberinto -> [Llave]
queLlavesDeboTener laberinto = sinRepetidos(llavesUtilizadasEn laberinto)

llavesUtilizadasEn :: Laberinto -> [Llave]
llavesUtilizadasEn Salida = []
llavesUtilizadasEn (Celda cofre) = llavesQueAbren cofre
llavesUtilizadasEn (Bifurcacion li ld) = queLlavesDeboTener li ++ queLlavesDeboTener ld

llavesQueAbren :: Cofre -> [Llave]
llavesQueAbren (Cofre keys _) = keys

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = 
    if (pertenece x xs)
        then sinRepetidos xs
        else x : sinRepetidos xs

pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = (n == x) || pertenece n xs

-----------------------------------------------------------------------------------------
cantidadDeOroCon :: [Llave] -> Laberinto -> Int
cantidadDeOroCon keys lab = oroQueSePuedeRecolectarCon keys lab 

oroQueSePuedeRecolectarCon :: [Llave] -> Laberinto -> Int 
oroQueSePuedeRecolectarCon [] lab = 0
oroQueSePuedeRecolectarCon keys (Salida) = 0
oroQueSePuedeRecolectarCon keys (Celda cofre) = if sePuedeAbrirConLlaves keys cofre 
                                                    then oroDisponibleEn cofre 
                                                    else 0
oroQueSePuedeRecolectarCon keys (Bifurcacion li ld) = oroQueSePuedeRecolectarCon keys li + oroQueSePuedeRecolectarCon keys ld

sePuedeAbrirConLlaves :: [Llave] -> Cofre -> Bool 
sePuedeAbrirConLlaves misKeys (Cofre keysNecesarias _ ) = alcanzanParaAbrir misKeys keysNecesarias 

alcanzanParaAbrir :: [Llave] -> [Llave] -> Bool 
alcanzanParaAbrir xs [] = True 
alcanzanParaAbrir xs (y:ys) = elem y xs && alcanzanParaAbrir xs ys

oroDisponibleEn :: Cofre -> Int 
oroDisponibleEn (Cofre _ oro) = oro

-----------------------------------------------------------------------------------------
haySalidaPor :: [Dir] -> Laberinto -> Bool
haySalidaPor [] laberinto = haySalidaEnEstePunto laberinto 
haySalidaPor (d:ds) Salida = error "Direcciones invalidas, sobran pasos"
haySalidaPor (d:ds) (Celda cofre) = False 
haySalidaPor (d:ds) (Bifurcacion li ld) =
    case d of 
        Izq -> haySalidaPor ds li
        Der -> haySalidaPor ds ld

haySalidaEnEstePunto :: Laberinto -> Bool 
haySalidaEnEstePunto (Salida) = True 
haySalidaEnEstePunto _ = False 

-----------------------------------------------------------------------------------------
--salidaMasCercana :: Laberinto -> [Dir]
