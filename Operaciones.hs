module Operaciones where
	import Validar
	import Simplificar 
    	     	
	--Funcion que deriva un solo termino
	derivarTermC :: Term ->Term
	derivarTermC (C((a,b),c)) = if (c == 0) then (C((0.0,0.0),0)) else (C((a * (read (show c) :: Double),b * (read (show c) :: Double)), c-1))
	
	--Funcion auxiliar para derivar
	derivarAux :: Polinomio ->Polinomio
	derivarAux [] = []
	derivarAux (x:xs) = [(derivarTermC x)] ++ (derivarAux xs)
	
	--Funcion que elimina los 0 de un polinomio
	eliminar0 :: Polinomio ->Polinomio
	eliminar0 [] = []
	eliminar0 (x:xs) = if (iguales (C((0.0,0.0),0))  x) then (eliminar0 xs) else ([x] ++ (eliminar0 xs))
	
	--Funcion que deriva un polinomio 
	derivar :: String ->Polinomio
	derivar p = eliminar0 (derivarAux (strToHaskell (p))) 
	
	--Funcion que integra un  solo termino
	integrarTerm :: Term ->Term
	integrarTerm (R (a,b)) = if (b == 0) then (R (a,1)) else (R ((a/(read (show (b+1)) :: Double)),b+1))
	integrarTerm (C ((a,b),c)) = if (c==0) then (C ((a,b),1)) else(C(((a/((read (show (c+1)) :: Double))),(b/((read (show (c+1)) :: Double)))),c+1))
	
	--Funcion que integra un polinomio
	integrar :: Polinomio ->Polinomio 
	integrar [] = []
	integrar (x:xs) = [(integrarTerm x)] ++ (integrar xs)
	
	--Funcion auxiliar para integrar de manera definida
	integrarDefAux :: Polinomio ->Double ->Double ->String
	integrarDefAux p a b = if a <= b then numCTostr(restNumC (evalPolAux3 (integrar p) (b,0.0)) (evalPolAux3 (integrar p) (a,0.0))) else numCTostr(restNumC (evalPolAux3 (integrar p) (a,0.0)) (evalPolAux3 (integrar p) (b,0.0))) 
	
	--Funcion que calcula la integral definida de un polinomio
	integrarDef :: String ->Double ->Double ->String
	integrarDef s a b = integrarDefAux (strToHaskell s) a b
	
	--Funcion que verifica si dos terminos son iguales
	iguales :: Term ->Term ->Bool
	iguales (R a) (C b) = False
	iguales (C a) (R b) = False
	iguales (R a) (R b) = a == b
	iguales (C a) (C b) = a == b
	
	--Funcion que suma dos numeros complejos
	sumNumC :: NumC ->NumC ->NumC
	sumNumC (a,b) (c,d) = (a+c,b+d) 
	
	--Funcion que resta dos numeros complejos
	restNumC :: NumC ->NumC ->NumC
	restNumC (a,b) (c,d) = (a-c,b-d)
	
	--Funcion que multiplica un numero complejo por un escalar
	mulNumCEs :: NumC ->Double ->NumC
	mulNumCEs (a,b) n = (a*n,b*n)
	  
	--Funcion que multiplica dos numeros complejos  
	mulNumC :: NumC ->NumC ->NumC
	mulNumC (a,b) (c,d) = ((a*c - b*d),(a*d+c*b))
	
	--Funcion que multiplica dos terminos complejos
	mulTC :: Term ->Term ->Term
	mulTC (C (a,b)) (C (x,y)) = C ((mulNumC a x), b+y)
	
	--Funcion que calcula la potencia de un numero complejo
	potNumC :: NumC ->Int ->NumC
	potNumC w 0 = (1,0)
	potNumC w n = (mulNumC w (potNumC w (n-1)))
	
	--Funcion que evalua un termino usando un numero complejos
	evalTerm :: Term ->NumC->Term
	evalTerm (R (a,b)) w = C ((mulNumCEs (potNumC w b) a),0)
	evalTerm (C (a,b)) w = C (((mulNumC (potNumC w b) a) ,0))    
	
	--Funcion auxiliar para evaluar un polinomio
	evalPolAux :: Polinomio ->NumC ->Polinomio
	evalPolAux [] w = []
	evalPolAux (x:xs) w = [(evalTerm x w)] ++ (evalPolAux xs w)
	
	--Funcion axiliar 2 para evaluar un polinomio
	evalPolAux2 :: String ->NumC ->NumC 
	evalPolAux2 s w = (termToNumC(head (simplificar (evalPolAux (strToHaskell s) w))))
	
	--Funcion axiliar 3 para evaluar un polinomio (se utiliza principalmente en integrarDefAux)
	evalPolAux3 :: Polinomio ->NumC ->NumC 
	evalPolAux3 p w = (termToNumC(head (simplificar (evalPolAux  p w))))
	
	--Funcion que tranforma un numero complejos a una cadena
	numCTostr :: NumC ->String
	numCTostr (a,b) = "(" ++ (show a) ++ "," ++ (show b) ++ ")"
	
	--Funcion que evalua un polinomio usando un numero complejo
	evalPol :: String ->NumC ->String
	evalPol s w = numCTostr (evalPolAux2 s w)
	
	--Funcion que devuelve el numero complejo de un termino
	termToNumC :: Term ->NumC
	termToNumC (R(a,b)) = (a,0.0)
	termToNumC (C ((a,b),c)) = (a,b)
	
	--Funcion que suma dos polinomios
	sumaPol :: String ->String ->Polinomio
	sumaPol p1 p2 = simplificar ((strToHaskell p1) ++ (strToHaskell p2))
	
	--Funcion que multiplica un termino por un polinomio
	mulTermPol :: Term ->Polinomio ->Polinomio
	mulTermPol t [] = []
	mulTermPol t (x:xs) = [(mulTC t x)] ++ (mulTermPol t xs)
	
	--Funcion auxiliar que multiplica dos polinomios
	mulPolsAux :: Polinomio ->Polinomio ->Polinomio
	mulPolsAux p1 [] = []
	mulPolsAux [] p2 = []
	mulPolsAux (x:xs) p2 = (mulTermPol x p2) ++ (mulPolsAux xs p2)
	
	--Funcion que multiplica dos polinomios
	mulPols :: String ->String ->Polinomio
	mulPols s1 s2 = simplificar (mulPolsAux (strToHaskell s1) (strToHaskell s2))       
