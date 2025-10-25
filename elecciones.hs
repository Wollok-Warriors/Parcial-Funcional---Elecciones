-- Utilizo sinónimos de tipo para modelar al país.
type Nombre = String
type Poblacion = Int
type Indicador = (String, Float)
type Estado = [Indicador]

data Pais = Pais {
    denominacion :: Nombre,
    cantidadHabitantes :: Poblacion,
    estado :: Estado
} deriving Show

-- País de ejemplo
miPais :: Pais
miPais = Pais {
    denominacion = "argentina",
    cantidadHabitantes = 40000000,
    estado = [("desocupacion", 12.4),
              ("educacion", 1000.0),
              ("deuda externa", 80000000.0),
              ("iva", 21.0),
              ("reservas", 30000000.0)] -- Nota: Asumo que el orden es el del ejemplo
}

-- | Función que permite que crezca la población del país durante un período
crecimientoVegetativo :: Pais -> Int -> Pais
crecimientoVegetativo pais cantidadPeriodos = 
    pais { cantidadHabitantes = nuevoValor }
  where 
    nuevoValor = round (fromIntegral (cantidadHabitantes pais) * (1.05 ** fromIntegral cantidadPeriodos))

-- | Función que analiza si el país está bien en un determinado momento si la deuda externa por habitante es mayor al iva vigente.
paisBien :: Pais -> Bool
paisBien pais = deudaPorHabitante > ivaVigente
    where 
        deudaPorHabitante = snd (estado pais !! 2) /  fromIntegral (cantidadHabitantes pais)
        ivaVigente = snd (estado pais !! 3)

-- | Función que permita ver si el país va a estar bien dentro de dos períodos, contemplando el crecimiento vegetativo e ignorando quien la gobierne. (Utilizar composición de funciones)
paisBienEnDosPeriodos :: Pais -> Bool
paisBienEnDosPeriodos = paisBien . (`crecimientoVegetativo` 2) -- Utilizo aplicación parcial

-- Hacer la función que representa un período de gobierno, que recibiendo un país y una fuerza política, 
--retorne el estado final del país, considerando también el crecimiento vegetativo. (Utilizar orden superior)

-- | Danielismo: Baja un punto la desocupacion y mejora la educación en un 10%.
danielismo :: Pais -> Pais
danielismo pais = paisPostDanielismo
    where 
        paisPostDanielismo = pais { 
               estado = [("desocupacion", snd (estado pais !! 0) - 1),
                        ("educacion", snd (estado pais !! 1) * 1.1),
                        ("deuda externa", snd (estado pais !! 2)),
                        ("iva", snd (estado pais !! 3)),
                        ("reservas", snd (estado pais !! 4))]
        }

-- | Margaritismo: Mejora la educación en un 40% y vuelve el iva al 18.
margaritismo :: Pais -> Pais
margaritismo pais = paisPostMargaritismo
    where
        paisPostMargaritismo = pais {
            estado = [
                ("desocupacion", snd (estado pais !! 0)),
                ("educacion", snd (estado pais !! 1)* 1.4),
                ("deuda externa", snd (estado pais !! 2)),
                ("iva", 18.0),
                ("reservas", snd (estado pais !! 4))
            ]
        }

-- | Mauricismo: Todo lo que empieza con 'd' lo duplica.
mauricismo :: Pais -> Pais
mauricismo pais = paisPostMauricismo 
    where 
        paisPostMauricismo = pais {
            estado = [
                ("desocupacion", snd (estado pais !! 0) * 2.0),
                ("educacion", snd (estado pais !! 1)),
                ("deuda externa", snd (estado pais !! 2) * 2.0),
                ("iva", snd (estado pais !! 3)),
                ("reservas", snd (estado pais !! 4))
            ]
        }

-- | Sergismo: Aumenta todo en un 10%
sergismo :: Pais -> Pais
sergismo pais = paisPostSergismo 
    where 
        paisPostSergismo = pais {
            estado = [
                ("desocupacion", snd (estado pais !! 0) * 1.10),
                ("educacion", snd (estado pais !! 1) * 1.10),
                ("deuda externa", snd (estado pais !! 2) * 1.10),
                ("iva", snd (estado pais !! 3) * 1.10),
                ("reservas", snd (estado pais !! 4) * 1.10)
            ]
        }

-- Nicolismo: Hace que la deuda externa se anule (que sea 0).
nicolismo :: Pais -> Pais
nicolismo pais = paisPostNicolismo 
    where 
        paisPostNicolismo = pais {
            estado = [
                ("desocupacion", snd (estado pais !! 0)),
                ("educacion", snd (estado pais !! 1)),
                ("deuda externa", 0),
                ("iva", snd (estado pais !! 3)),
                ("reservas", snd (estado pais !! 4))
            ]
        }

-- | Adolfismo: Deja todo igual
adolfismo :: Pais -> Pais
adolfismo pais = pais 

-- | Nueva fuerza política: Mileismo -> aumenta la educación en un 10% y baja la deuda externa un 15%
mileismo :: Pais -> Pais
mileismo pais = paisPostMileismo 
    where 
        paisPostMileismo = pais {
            estado = [
                ("desocupacion", snd (estado pais !! 0)),
                ("educacion", snd (estado pais !! 1)*1.1),
                ("deuda externa", snd (estado pais !! 2) - snd(estado pais !! 2) * 0.15),
                ("iva", snd (estado pais !! 3)),
                ("reservas", snd (estado pais !! 4))
            ]
        }

-- | Función que representa un período de gobierno
periodoDeGobierno :: Pais -> (Pais -> Pais) -> Pais
periodoDeGobierno pais fuerza =  crecimientoVegetativo (fuerza pais) 1 -- Función de orden superior pues recibe una función como parámetro

-- | Función que determina como va a quedar el pais luego de un determinado gobierno (asumo que "como va a quedar el pais" es devolver el pais post gobierno)
paisPostGobierno :: Pais -> (Pais -> Pais) -> Pais
paisPostGobierno pais fuerza
    | paisBien paisTrasPeriodo = paisPostGobierno paisTrasPeriodo fuerza  -- Si el pais termina bien, el gobierno es reelegido
    | otherwise = paisTrasPeriodo
  where
    paisTrasPeriodo = periodoDeGobierno pais fuerza
