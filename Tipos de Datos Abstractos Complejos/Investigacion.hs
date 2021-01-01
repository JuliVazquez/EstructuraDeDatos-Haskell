
data Investigacion = ConsI (Map Nombre Persona) (Map Evidencia [Nombre]) (PriorityQueue Persona) Int

-----------------------------------------------------------------------------
comenzarInvestigacion :: Investigacion
comenzarInvestigacion = ConsI emptyM emptyM emptyPQ 0

-----------------------------------------------------------------------------
cantEvidenciaIngresada :: Investigacion -> Int
cantEvidenciaIngresada (ConsI mapP mapE pqP cantE) = cantE 

-----------------------------------------------------------------------------
evidenciaIngresada :: Investigacion -> [Evidencia]
evidenciaIngresada (ConsI mapP mapE pqP cantE) = domM mapE

-----------------------------------------------------------------------------
nombresIngresados :: Investigacion -> [Nombre]
nombresIngresados (ConsI mapP mapE pqP cantE) = domM mapP

-----------------------------------------------------------------------------
casoCerrado :: Investigacion -> Bool
casoCerrado (ConsI mapP mapE pqP cantE) = cantEvidencia (maxPQ pqP) >= 5

-----------------------------------------------------------------------------
esSospechoso :: Nombre -> Investigacion -> Bool
esSospechoso nomP (ConsI mapP mapE pqP cantE) = cantEvidencia (fromJust(lookupM nomP mapP)) == 0

fromJust :: Maybe a -> a 
fromJust (Just x) = x

-----------------------------------------------------------------------------
posiblesInocentes :: Investigacion -> [Persona]
posiblesInocentes (ConsI mapP mapE pqP cantE) = posiblesInocentesPq pqP 

posiblesInocentesPq :: PriorityQueue Persona -> [Persona]
posiblesInocentesPq pqPersonas = if cantEvidencia(maxPQ pqPersonas) == 0
                                    then (maxPQ pqPersonas) : posiblesInocentesPq (deleteMaxPQ pqPersonas)
                                    else posiblesInocentesPq (deleteMaxPQ pqPersonas)

-----------------------------------------------------------------------------
ingresarPersonas :: [Nombre] -> Investigacion -> Investigacion
ingresarPersonas nombresNuevos (ConsI mapP mapE pqP cantE) = ConsI (actualizarMapP nombresNuevos mapP) mapE(actualizarPqP nombresNuevos pqP) cantE 

actualizarMapP :: [Nombre] -> Map Nombre Persona -> Map Nombre Persona
actualizarMapP [] mapP = mapP 
actualizarMapP (n:ns) mapP = assocM n (crearP n) (actualizarMapP ns mapP)

actualizarPqP :: [Nombre] -> PriorityQueue Persona -> PriorityQueue Persona
actualizarPqP [] pqP = pqP 
actualizarPqP (n:ns) pqP = insertPQ (crearP n) (actualizarPqP ns pqP)

-----------------------------------------------------------------------------
ingresarEvidencia :: Evidencia -> Nombre -> Investigacion -> Investigacion
ingresarEvidencia evidencia nombre (ConsI mapP mapE pqP cantE) = 
    ConsI (agregarEvidenciaParaEnMapP nombre evidencia mapP) 
            (agregarNombreAEvidenciaEnMapE nombre evidencia mapP) 
                (agregarEvidenciaParaEnPqP (fromJust(lookupM nombre mapP)) evidencia pqP) (cantE + 1)

agregarEvidenciaParaEnMapP :: Nombre -> Evidencia -> Map Nombre Persona -> Map Nombre Persona 
agregarEvidenciaParaEnMapP nombre evidencia mapP = assocM nombre (agregarEvidencia evidencia (fromJust(lookupM nombre mapP))) mapP

agregarNombreAEvidenciaEnMapE :: Nombre -> Evidencia -> Map Evidencia [Nombre] -> Map Evidencia [Nombre]
agregarNombreAEvidenciaEnMapE nombre evidencia mapE = 
    case lookupM evidencia mapE of
        Just nombres    -> assocM eviencia (nombre:nombres) mapE
        Nothing         -> assocM evidencia [nombre] mapE

agregarEvidenciaParaEnPqP :: Persona -> Evidencia -> PriorityQueue Persona -> PriorityQueue Persona 
agregarEvidenciaParaEnPqP persona evidencia pqPersonas = 
    if persona == maxPQ pqPersonas
        then insertPQ (agregarEvidencia persona) (deleteMaxPQ pqPersonas)
        else insertPQ (maxPQ pqPersonas) (agregarEvidenciaParaEnPqP persona eviencia (deleteMaxPQ pqPersonas))

-----------------------------------------------------------------------------
comenzarConPersonas :: [Nombre] -> Investigacion
comenzarConPersonas nombres = ingresarPersonas nombres (comenzarInvestigacion)

-----------------------------------------------------------------------------
todosInocentes :: Investigacion -> Bool
todosNombresInocentes investigacion = cantEvidenciaIngresada == 0

-----------------------------------------------------------------------------
terminaCerrado :: [(Evidencia, Nombre)] -> Investigacion -> Bool
terminaCerrado evidenciasNuevas investigacion = casoCerrado (investigacionConNuevasEvidencias evidenciasNuevas investigacion)

investigacionConNuevasEvidencias :: [(Evidencia, Nombre)] -> Investigacion -> Investigacion
investigacionConNuevasEvidencias [] investigacion = investigacion
investigacionConNuevasEvidencias (e:es) investigacion = ingresarEvidencia (fst e) (snd e) (investigacionConNuevasEvidencias es investigacion)