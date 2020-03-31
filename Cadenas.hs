module Cadenas where
	import Validar
	import Simplificar
	import Operaciones
	import Data.List
	
	--Funcion que tranforma un termino a una reprsentacion en cadena
	termToStringAux :: Term ->String
	termToStringAux (R(a,b)) = show a
	termToStringAux (C((x,y),z)) = if y == 0 then termToStringAux (R (x,z)) else termCToString (C((x,y),z))
	
	--Funcio auxiliar para termToStringAux
	termCToString :: Term ->String
	termCToString (C((x,y),z)) = if y >= 0 then "(" ++ (show x) ++ "+" ++ (show y) ++ "i"++ ")"else "(" ++ (show x) ++ (show y) ++ "i"++ ")" 
        	 
	--Funcion auxiliar que transforma terminos complejos en reales, solo si es necesario
	termToTerm :: Term ->Term
	termToTerm (R (a,b)) = (R (a,b))
	termToTerm (C((a,0.0),c)) = R(a,c)
	termToTerm (C((a,b),c)) = (C((a,b),c))
	
	--Transforma un polinomio de solo complejos a uno mixto
	mixPol :: Polinomio ->Polinomio
	mixPol [] = []
	mixPol (x:xs) = [(termToTerm x)] ++ (mixPol xs)
	
	--Funcion que verifica si pondermos un mas o no 
	masTerm :: Term ->String
	masTerm (R(a,b)) = if a < 0 then "" else " + "
	masTerm (C((a,b),c)) = " + "
	 
	--Funcion auxiliar que transforma un polinomio a una representacion en cadena
	polToStringAux :: Polinomio ->String
	polToStringAux [] = ""
	polToStringAux [t] = (termToString t)
	polToStringAux (x:xs) = ((termToString x) ++ (masTerm x)) ++ (polToString xs) 
	
	--Funcion auxiliar 2 que tranforma un polinomio a una representacion en cadena
	polToString :: Polinomio ->String
	polToString p = (polToStringAux (mixPol p))
        
    --Funcion que tranforma un termino a una reprsentacion en cadena
	termToString :: Term ->String
	termToString (R(a,b)) = (termToStringAux (R(a,b))) ++ (termAux (R(a,b))) 
	termToString (C((x,y),z)) = (termToStringAux (C((x,y),z))) ++ (termAux (C((x,y),z)))
	
	    --Funcion auxiliar para termToString
        termAux :: Term ->String
        termAux (R(a,0)) = ""
        termAux (R(a,1)) = "x"
        termAux (R(a,b)) = "x^" ++ (show b)
        termAux (C((x,y),0)) = ""
        termAux (C((x,y),1)) = "x"
        termAux (C((x,y),z)) = "x^"++ (show z)
