--REGISTROS 

data Persona = ConsP String Int 
    deriving (Show)

p1 :: Persona 
p1 = ConsP "Jotaro" 28

p2 :: Persona 
p2 = ConsP "Joseph" 79

p3 :: Persona 
p3 = ConsP "Josuke" 16

p4 :: Persona 
p4 = ConsP "Kira" 33

diu :: [Persona]
diu = [p1,p2,p3,p4]

------------------------------------------------
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA n [] = []
mayoresA n (x:xs) = if esMayorA x n 
                        then x : mayoresA n xs 
                        else mayoresA n xs    

esMayorA :: Persona -> Int -> Bool 
esMayorA (ConsP _ e) n = e > n

------------------------------------------------
promedioEdad :: [Persona] -> Int
promedioEdad [] = error "Lista vacia"
promedioEdad xs = promedio(listaDeEdades xs)

listaDeEdades :: [Persona] -> [Int]
listaDeEdades [] = []
listaDeEdades (x:xs) = edadDe x : listaDeEdades xs 

edadDe :: Persona -> Int 
edadDe (ConsP _ e) = e 

promedio :: [Int] -> Int 
promedio [] = 0
promedio (xs) = div (sumatoria xs) (longitud xs)

longitud :: [a] -> Int 
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

sumatoria :: [Int] -> Int 
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

------------------------------------------------
elMasViejo :: [Persona] -> Persona
elMasViejo [] = error "Lista vacia"
elMasViejo (x:[]) = x 
elMasViejo (x:xs) = elMayorEntre x (elMasViejo xs)

elMayorEntre :: Persona -> Persona -> Persona
elMayorEntre (ConsP ni ei) (ConsP nd ed) =  if ei >= ed 
                                                then ConsP ni ei 
                                                else ConsP nd ed    

-----------------------------------------------------------------------------------------------------
data TipoDePokemon = Agua | Fuego | Planta
    deriving (Show, Eq)
data Pokemon = ConsPoke String TipoDePokemon Int 
    deriving (Show)
data Entrenador = ConsEnt String [Pokemon]
    deriving (Show)

charmander :: Pokemon 
charmander = ConsPoke "Charmander" Fuego 15
squirtle :: Pokemon 
squirtle = ConsPoke "Squirtle" Agua 16
bulbasaur :: Pokemon 
bulbasaur = ConsPoke "Bulbasaur" Planta 14

ash :: Entrenador 
ash = ConsEnt "Ash" [charmander, bulbasaur]
gary :: Entrenador 
gary = ConsEnt "Gary" [squirtle, squirtle]

------------------------------------------------
cantPokemones :: Entrenador -> Int
cantPokemones (ConsEnt _ xs) = longitud xs

------------------------------------------------
cantPokemonesDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonesDe t (ConsEnt _ xs) = cantDelTipo t xs 

cantDelTipo :: TipoDePokemon -> [Pokemon] -> Int 
cantDelTipo t [] = 0
cantDelTipo t (x:xs) =  if tipoDe x == t 
                            then 1 + cantDelTipo t xs 
                            else cantDelTipo t xs 

tipoDe :: Pokemon -> TipoDePokemon 
tipoDe (ConsPoke n t p) = t

-----------------------------------------------------------------------------------------------------
data Seniority = Junior | SemiSenior | Senior 
    deriving (Show, Eq)

data Proyecto = ConsProyecto String
    deriving (Show,Eq)
    
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
    deriving (Show)
data Empresa = ConsEmpresa [Rol]
    deriving (Show)

phoenix :: Proyecto 
phoenix = ConsProyecto "Phoenix"
gobi :: Proyecto 
gobi = ConsProyecto "GOBI"
metalica :: Proyecto 
metalica = ConsProyecto "METALICA"

r1 :: Rol 
r1 = Developer Junior phoenix
r2 :: Rol 
r2 = Developer Senior phoenix
r3 :: Rol 
r3 = Management Senior phoenix
r4 :: Rol 
r4 = Developer SemiSenior gobi
r5 :: Rol 
r5 = Management SemiSenior gobi
r6 :: Rol 
r6 = Developer Junior metalica
r7 :: Rol 
r7 = Developer Senior metalica
r8 :: Rol 
r8 = Developer SemiSenior metalica

empresa :: Empresa 
empresa = ConsEmpresa [r1,r2,r3,r4,r5,r6,r7,r8]

------------------------------------------------
proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa xs) = sinRepetidos(listaDeProyectos xs)

listaDeProyectos :: [Rol] -> [Proyecto]
listaDeProyectos [] = []
listaDeProyectos (x:xs) = proyectoDe x : listaDeProyectos xs

proyectoDe :: Rol -> Proyecto
proyectoDe (Developer _ p) = p
proyectoDe (Management _ p) = p

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) =   if (pertenece x xs)
                            then sinRepetidos xs 
                            else x : sinRepetidos xs 

pertenece :: Eq a => a -> [a] -> Bool 
pertenece e [] = False
pertenece e (x:xs) = (e == x) || pertenece e xs

------------------------------------------------
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn [] e = 0
cantQueTrabajanEn proyectos (ConsEmpresa roles) = cantRolesQueTrabajanEn roles proyectos 

cantRolesQueTrabajanEn :: [Rol] -> [Proyecto] -> Int 
cantRolesQueTrabajanEn [] proyectos = 0
cantRolesQueTrabajanEn (r:rs) proyectos =   if trabajaEnAlguno r proyectos
                                                then 1 + cantRolesQueTrabajanEn rs proyectos
                                                else cantRolesQueTrabajanEn rs proyectos

trabajaEnAlguno :: Rol -> [Proyecto] -> Bool 
trabajaEnAlguno r [] = False 
trabajaEnAlguno r (p:ps) = esParteDelProyecto r p || trabajaEnAlguno r ps                    

esParteDelProyecto :: Rol -> Proyecto -> Bool 
esParteDelProyecto (Developer _ p) proyecto = p == proyecto
esParteDelProyecto (Management _ p) proyecto = p == proyecto

------------------------------------------------
losDevSenior :: Empresa -> Int
losDevSenior (ConsEmpresa roles) = losDevSeniorRoles roles

losDevSeniorRoles :: [Rol] -> Int 
losDevSeniorRoles [] = 0
losDevSeniorRoles (r:rs) =   if( esDevSenior r)
                            then 1 + losDevSeniorRoles rs 
                            else losDevSeniorRoles rs 

esDevSenior :: Rol -> Bool 
esDevSenior (Developer s _) = s == Senior
esDevSenior (Management _ _) = False