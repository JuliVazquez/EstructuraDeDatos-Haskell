--IMPLEMENTACION COMO USUARIO DE RONDE DE MATE

--Dada una ronda de mate, computa la lista de las personas ordenadas de menor a mayor según la cantidad de mates que tomaron.
personasOrdenadas :: RondaMate -> [Persona]
personasOrdenadas rondaDeMate = if cantidadDePersonas rondaDeMate > 0
                                    then fst(quienMenosTomo rondaDeMate) : personasOrdenadas (quitar(fst(quienMenosTomo rondaDeMate) rondaDeMate))
                                    else []

--------------------------------------------------------------------------------------
--IMPLEMENTACION DE LA INTERFAZ RONDA DE MATE

data RondaMate = RM (Ronda Persona) (MultiSet Persona) Int


nuevaRondaMate :: Int -> [Persona] -> RondaMate
nuevaRondaMate n personas =  RM (nuevaRonda personas) (agregarPersonasAMS personas) n 

agregarPersonasAMS :: [Persona] -> MultiSet Persona
agregarPersonasAMS [] = emptyMS
agregarPersonasAMS (p:ps) = addMS p (agregarPersonasAMS ps)

-----------------------------------------------------------------------------------------------
cebar :: RondaMate -> RondaMate
cebar (RM ronda personas 0) = (RM ronda personas 0)
cebar (RM ronda personas matesRestantes) = RM (snd(siguiente ronda)) (agregarMateAlQueToma (fst(siguiente ronda)) personas) (matesRestantes-1) 

agregarMateAlQueToma :: Persona -> MultiSet Persona -> MultiSet Persona 
agregarMateAlQueToma persona msPersonas = addMS persona msPersonas

-----------------------------------------------------------------------------------------------
saltear :: Int -> RondaMate -> (RondaMate,[Persona])
saltear 0 (RM ronda personas matesRestantes) = RM (snd(siguiente ronda)) (agregarMateAlQueToma (fst(siguiente ronda)) personas) (matesRestantes-1)
saltear n (RM ronda personas matesRestantes) = saltear (n-1) (RM (snd(siguiente ronda)) personas matesRestantes)

-----------------------------------------------------------------------------------------------
quitar :: Persona -> RondaMate -> RondaMate 
