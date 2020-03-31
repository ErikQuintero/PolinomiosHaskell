module Polinomios where
	import Validar
	import Simplificar
	import Operaciones
	import Cadenas
	import Data.List
	
	derivar :: String ->String
	derivar s = "La derivada es: "++ polToString (derivar1 s)
	
	integrar :: String ->String
	integrar s = "La integral es: "++ polToString (integrar1 (strToHaskell s))
	
	integrarDef :: String ->Double ->Double ->String
	integrarDef s a b = "El resultado es: "++ (integrarDef1 s a b) 
	
	sumaPol :: String ->String ->String
	sumaPol s1 s2 = "El resultado de la suma es: "++ polToString (sumaPol1 s1 s2)
	
	mulPol :: String ->String ->String
	mulPol s1 s2 = "El resultado de la multiplicacion es: "++ polToString (mulPols1 s1 s2) 
