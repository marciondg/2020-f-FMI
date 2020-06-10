module Lib where
import Text.Show.Functions

laVerdad = True
type Dinero = Float
type Recurso = String
    
{- 
========Punto 1 A========
Representar el TAD País. 
-}
data Pais = Pais{
    ingresoPerCapita :: Dinero,
    poblacionActivaPublico :: Int,
    poblacionActivaPrivado :: Int,
    recursosNaturales :: [Recurso],
    deudaConFMI :: Dinero
} deriving Show

{- 
========Punto 1 B========
Dar un ejemplo de cómo generar al país Namibia, cuyo ingreso per cápita es de 4140 u$s, la población activa del sector público es de 400.000, 
la población activa del sector privado es de 650.000, su riqueza es la minería y el ecoturismo y le debe 50 (millones de u$s) al FMI.
-}
namibia :: Pais
namibia = Pais 4140 400000 650000 ["mineria", "ecoturismo"] 50000000

{- 
========Punto 2========
Implementar las estrategias que forman parte de las recetas del FMI.  
El FMI es especialista en dar recetas. Cada receta combina una o más estrategias que se describen a continuación:
+-+-+-+-+-+-+-+-+-+-+-+
Prestarle n millones de dólares al país, esto provoca que el país se endeude en un 150% de lo que el FMI le presta (por los intereses)
-}
type EstrategiaFMI = Pais -> Pais
prestarMillonesDolares :: Dinero->EstrategiaFMI
prestarMillonesDolares n = endeudar (n*1000000*1.5) 

endeudar :: Dinero->Pais->Pais
endeudar deuda pais = pais {deudaConFMI = deudaConFMI pais + deuda}

{-+-+-+-+-+-+-+-+-+-+-+-+
reducir x cantidad de puestos de trabajo del sector público, lo que provoca que se reduzca la cantidad de activos en el sector público y 
además que el ingreso per cápita disminuya en 20% si los puestos de trabajo son más de 100 ó 15% en caso contrario
-}

