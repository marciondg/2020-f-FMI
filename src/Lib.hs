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
prestarMillonesDolares n = endeudar $ ((*1.5).millones) n

cambiarDeuda :: (Dinero->Dinero->Dinero)->Dinero->Pais->Pais
cambiarDeuda f dinero pais = pais {deudaConFMI = (deudaConFMI pais) `f` dinero}

endeudar :: Dinero->Pais->Pais
endeudar dinero = cambiarDeuda (+) dinero

millones :: Dinero -> Dinero
millones = (*1000000)
{-+-+-+-+-+-+-+-+-+-+-+-+
Reducir x cantidad de puestos de trabajo del sector público, lo que provoca que se reduzca la cantidad de activos en el sector público y 
además que el ingreso per cápita disminuya en 20% si los puestos de trabajo son más de 100 ó 15% en caso contrario
-}

reducirPuestosTrabajoPublico :: Int->EstrategiaFMI
reducirPuestosTrabajoPublico puestosAQuitar = (reducirIngresoPerCapita puestosAQuitar.reducirActivosPublico puestosAQuitar) 

reducirActivosPublico :: Int->Pais->Pais
reducirActivosPublico puestos pais = pais {poblacionActivaPublico = poblacionActivaPublico pais - puestos}

reducirIngresoPerCapita :: Int->Pais->Pais
reducirIngresoPerCapita puestos pais | puestos > 100 = pais {ingresoPerCapita = ((*0.8).ingresoPerCapita) pais}
                                     | otherwise = pais {ingresoPerCapita = ((*0.75).ingresoPerCapita) pais}

{-+-+-+-+-+-+-+-+-+-+-+-+
Darle a una empresa afín al FMI la explotación de alguno de los recursos naturales, 
esto disminuye 2 millones de dólares la deuda que el país mantiene con el FMI pero también deja momentáneamente 
sin recurso natural a dicho país. No considerar qué pasa si el país no tiene dicho recurso. 
-}

permitirExplotacionDeRecursoNatural :: Recurso->EstrategiaFMI
permitirExplotacionDeRecursoNatural recurso = (regularizarDeuda $ millones 2).perderRecurso recurso

regularizarDeuda :: Dinero->Pais->Pais
regularizarDeuda dinero = cambiarDeuda (-) dinero

perderRecurso :: Recurso->Pais->Pais
perderRecurso recurso pais = pais {recursosNaturales= restoRecursos recurso (recursosNaturales pais)}

restoRecursos :: Recurso->[Recurso]->[Recurso]
restoRecursos recursoASacar listaRecursos = filter (recursoDiferente recursoASacar) listaRecursos

recursoDiferente :: Recurso->Recurso->Bool
recursoDiferente recurso1 recurso2 = recurso1 /= recurso2

{-+-+-+-+-+-+-+-+-+-+-+-+
Establecer un “blindaje”, lo que provoca prestarle a dicho país la mitad de su Producto Bruto Interno 
(que se calcula como el ingreso per cápita multiplicado por su población activa, sumando puestos públicos y privados de trabajo) 
y reducir 500 puestos de trabajo del sector público. Evitar la repetición de código. -}

darBlindaje :: EstrategiaFMI
darBlindaje pais = (reducirPuestosTrabajoPublico 500.prestarMillonesDolares(pbi pais / millones 2)) pais


pbi :: Pais->Dinero
pbi pais = ingresoPerCapita pais * fromIntegral(poblacionActiva pais)

poblacionActiva :: Pais->Int
poblacionActiva pais = poblacionActivaPrivado pais + poblacionActivaPublico pais

{- 
========Punto 3========
Modelar una receta que consista en prestar 200 millones, y darle a una empresa X la explotación de la “Minería” de un país.
-}
type Receta = EstrategiaFMI

receta :: Receta
receta = (permitirExplotacionDeRecursoNatural "mineria".prestarMillonesDolares 200)

{- 
Ahora queremos aplicar la receta del punto 3.a al país Namibia (creado en el punto 1.b). Justificar cómo se logra el efecto colateral.
-}

