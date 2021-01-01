--IMPLEMENTACIONES COMO USUARIO DEL TAD ORGANIZADOR

programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum
programasEnComun p1 p2 org = intersection (programasDe p1) (programasDe p2)

--------------------------------------------------------------------------------
esUnGranHacker :: Organizador -> Persona -> Bool
esUnGranHacker organizador persona = length(todosLosProgramas organizador) == nroProgramasDePersona organizado persona

--------------------------------------------------------------------------------
elMayorPrograma :: Organizador -> Maybe Checksum
elMayorPrograma organizador =  = elProgramaMasGrandeDe(todosLosProgramas organizador) organizador

elProgramaMasGrandeDe :: [Checksum] -> Organizador -> Checksum 
elProgramaMasGrandeDe [] organizador =  Nothing 
elProgramaMasGrandeDe (c:cs) organizador =  Just(max(sizeS(autoresDe organizador c) (elProgramaMasGrandeDe cs organizador)))


--------------------------------------------------------------------------------
--IMPLEMENTACION DE LA INTERFAZ DEL TAD ORGANIZADOR

data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum))

nuevo :: Organizador
nuevo = MkO emptyM emptyM

--------------------------------------------------------------------------------
agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
agregarPrograma (MkO mapCks mapPers) nChecksum setPersonas = 
    MkO (assocM nChecksum setPersonas mapCks) (asociarCheacksumAPersonasM nChecksum (set2list setPersonas) mapPers)

asociarCheacksumAPersonasM :: Checksum -> [Persona] -> Map Persona (Set Checksum) -> Map Persona (Set Checksum)
asociarCheacksumAPersonasM nChecksum [] mapPersonas = mapPersonas
asociarCheacksumAPersonasM nChecksum (p:ps) mapPersonas =
     assocM p (addS nChecksum (fromJust(lookupM p mapPersonas))) (asociarCheacksumAPersonasM ps mapPersonas)

fromJust :: Maybe a -> a 
fromJust (Just x) = x

--------------------------------------------------------------------------------
todosLosProgramas :: Organizador -> [Checksum]
todosLosProgramas (MkO mapCks mapPers) = domM mapCks

--------------------------------------------------------------------------------
autoresDe :: Organizador -> Checksum -> Set Persona
autoresDe (MkO mapCks mapPers) cksBusado = fromJust(lookupM cksBuscado mapCks)

--------------------------------------------------------------------------------
programasDe :: Organizador -> Persona -> Set Checksum
programasDe (MkO mapCks mapPers) persona = fromJust(lookupM persona mapPers)

--------------------------------------------------------------------------------
programaronJuntas :: Organizador -> Persona -> Persona -> Bool
programaronJuntas organizador pers1 pers2 = not (isEmptyS (intersection (safeProgramasDe organizador pers1) (safeProgramasDe organizador pers2)))

safeProgramasDe :: Organizador -> Persona -> Set Checksum
programasDe (MkO mapCks mapPers) persona = 
    case lookupM persona mapPers of 
        Just setCks -> setCks
        Nothing     -> emptyS

--------------------------------------------------------------------------------
nroProgramasDePersona :: Organizador -> Persona -> Int
nroProgramasDePersona (MkO mapCks mapPers) persona = 
    case lookupM persona mapPers of 
        Just setCks -> sizeS(setCks)
        Nothing     -> 0