module Polinomios where
	import Validar
	import Simplificar
	import Operaciones
	import Cadenas
	import Data.List
	
	--Funcion que deriva un polinomio
	derivar :: String ->String
	derivar s = if ((valTotal s) == False) then error "Polinomio invalido" else "La derivada es: "++ polToString (derivar1 s)
	
	--Funcion que integra un polinomio
	integrar :: String ->String
	integrar s = if ((valTotal s) == False) then error "Polinomio invalido" else "La integral es: "++ polToString (integrar1 (strToHaskell s))
	
	--Funcion que integra de manera definida un polinomio
	integrarDef :: String ->Double ->Double ->String
	integrarDef s a b = if ((valTotal s) == False) then error "Polinomio invalido" else "El resultado es: "++ (integrarDef1 s a b) 
	
	--Funcion que suma dos polinomios un polinomio
	sumaPol :: String ->String ->String
	sumaPol s1 s2 = if (((valTotal s1) == False) && ((valTotal s2) == False)) then error "Polinomios invalidos" else "El resultado de la suma es: "++ polToString (sumaPol1 s1 s2)
	
	--Funcion que multiplica dos polinomios un polinomio
	mulPol :: String ->String ->String
	mulPol s1 s2 = if (((valTotal s1) == False) && ((valTotal s2) == False)) then error "Polinomios invalidos" else "El resultado de la multiplicacion es: "++ polToString (mulPols1 s1 s2) 
