-- TIPOS RECURSIVOS 
--PIZZAS 

data Pizza =    Prepizza
                | Capa Ingrediente Pizza
    deriving (Show)

data Ingrediente =  Salsa
                    | Queso
                    | Jamon
                    | Aceitunas Int
    deriving (Show)

-------------------------------------------------------------
pizza1 :: Pizza 
pizza1 = Capa (Aceitunas 8) (Capa Jamon (Capa Queso (Capa Queso (Capa Salsa Prepizza))))

pizza2 :: Pizza 
pizza2 = Prepizza

pizza3 :: Pizza 
pizza3 = Capa Queso (Capa Queso Prepizza)

pizza4 :: Pizza 
pizza4 = Capa Queso (Capa Salsa (Capa Salsa Prepizza))

ingredientes1 :: [Ingrediente]
ingredientes1 = [Queso, Queso, Salsa, Salsa]

listaDePizzas :: [Pizza]
listaDePizzas = [pizza1, pizza2, pizza3, pizza4]
-------------------------------------------------------------

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa ing restoCapas) = 1 + cantidadDeCapas restoCapas

-------------------------------------------------------------
armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (i:is) = Capa i (armarPizza is)

-------------------------------------------------------------
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza 
sacarJamon (Capa i restoCapas) =    if esJamon i 
                                        then sacarJamon restoCapas
                                        else Capa i (sacarJamon restoCapas)

esJamon :: Ingrediente -> Bool 
esJamon Jamon = True 
esJamon _ = False

-------------------------------------------------------------
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso pizza = tieneQueso pizza && tieneSalsa pizza && (not(tieneJamon pizza)) && (not(tieneAceitunas pizza))

tieneQueso :: Pizza -> Bool 
tieneQueso Prepizza = False
tieneQueso (Capa i restoCapas) = esQueso i || tieneQueso restoCapas

tieneSalsa :: Pizza -> Bool 
tieneSalsa Prepizza = False
tieneSalsa (Capa i restoCapas) = esSalsa i || tieneSalsa restoCapas

tieneJamon :: Pizza -> Bool 
tieneJamon Prepizza = False
tieneJamon (Capa i restoCapas) = esJamon i || tieneJamon restoCapas

tieneAceitunas :: Pizza -> Bool 
tieneAceitunas Prepizza = False
tieneAceitunas (Capa i restoCapas) = esAceituna i || tieneAceitunas restoCapas

esQueso :: Ingrediente -> Bool 
esQueso Queso = True 
esQueso _ = False

esSalsa :: Ingrediente -> Bool 
esSalsa Salsa = True 
esSalsa _ = False

esAceituna :: Ingrediente -> Bool 
esAceituna (Aceitunas _) = True 
esAceituna _ = False

-------------------------------------------------------------
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza 
duplicarAceitunas (Capa i restoCapas) = if esAceituna i 
                                            then Capa (duplicarCapaDeAceituna i) (duplicarAceitunas restoCapas)
                                            else Capa i (duplicarAceitunas restoCapas)

duplicarCapaDeAceituna :: Ingrediente -> Ingrediente 
duplicarCapaDeAceituna (Aceitunas cantidad) = Aceitunas (cantidad*2)

-------------------------------------------------------------
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza [] = []
cantCapasPorPizza (p:ps) = (cantidadDeIngredienteDe p, p) : cantCapasPorPizza ps

cantidadDeIngredienteDe :: Pizza -> Int 
cantidadDeIngredienteDe pizza = longitudSinRepetidos (ingredientesUsados pizza)

ingredientesUsados :: Pizza -> [Ingrediente]
ingredientesUsados Prepizza = []
ingredientesUsados (Capa i restoCapas) =  i : ingredientesUsados restoCapas

longitudSinRepetidos :: [Ingrediente] -> Int
longitudSinRepetidos [] = 0
longitudSinRepetidos (i:is) =   if pertenece i is 
                                    then longitudSinRepetidos is 
                                    else 1 + longitudSinRepetidos is       

pertenece :: Ingrediente -> [Ingrediente] -> Bool
pertenece e [] = False
pertenece e (x:xs) = sonElMismoIngrediente e x || pertenece e xs    

sonElMismoIngrediente :: Ingrediente -> Ingrediente -> Bool
sonElMismoIngrediente Queso Queso = True
sonElMismoIngrediente Salsa Salsa = True
sonElMismoIngrediente Jamon Jamon = True
sonElMismoIngrediente _ _ = False
