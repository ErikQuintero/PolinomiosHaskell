module Operar where
	import Validar
	import Simplificar 
    	     	
	derivarTermC :: Term ->Term
	derivarTermC (C((a,b),c)) = if (c == 0) then (C((0.0,0.0),0)) else (C((a * (read (show c) :: Double),b * (read (show c) :: Double)), c-1))
	
	derivarAux :: Polinomio ->Polinomio
	derivarAux [] = []
	derivarAux (x:xs) = [(derivarTermC x)] ++ (derivarAux xs)
	
	eliminar0 :: Polinomio ->Polinomio
	eliminar0 [] = []
	eliminar0 (x:xs) = if (iguales (C((0.0,0.0),0))  x) then (eliminar0 xs) else ([x] ++ (eliminar0 xs))
	
	derivar :: String ->Polinomio
	derivar p = eliminar0 (derivarAux (strToHaskell (p))) 
	
	iguales :: Term ->Term ->Bool
	iguales (R a) (C b) = False
	iguales (C a) (R b) = False
	iguales (R a) (R b) = a == b
	iguales (C a) (C b) = a == b
	
	sumNumC :: NumC ->NumC ->NumC
	sumNumC (a,b) (c,d) = (a+c,b+d) 
	
	mulNumCEs :: NumC ->Double ->NumC
	mulNumCEs (a,b) n = (a*n,b*n)
	  
	mulNumC :: NumC ->NumC ->NumC
	mulNumC (a,b) (c,d) = ((a*c - b*d),(a*d+c*b))
	
	mulTC :: Term ->Term ->Term
	mulTC (C (a,b)) (C (x,y)) = C ((mulNumC a x), b+y)
	
	potNumC :: NumC ->Int ->NumC
	potNumC w 0 = (1,0)
	potNumC w n = (mulNumC w (potNumC w (n-1)))
	
	evalTerm :: Term ->NumC->Term
	evalTerm (R (a,b)) w = C ((mulNumCEs (potNumC w b) a),0)
	evalTerm (C (a,b)) w = C (((mulNumC (potNumC w b) a) ,0))    
	
	evalPolAux :: Polinomio ->NumC ->Polinomio
	evalPolAux [] w = []
	evalPolAux (x:xs) w = [(evalTerm x w)] ++ (evalPolAux xs w)
	
	evalPolAux2 :: String ->NumC ->NumC 
	evalPolAux2 s w = (termToNumC(head (simplificar (evalPolAux (strToHaskell s) w))))
	
	numCTostr :: NumC ->String
	numCTostr (a,b) = "(" ++ (show a) ++ "," ++ (show b) ++ ")"
	
	evalPol :: String ->NumC ->String
	evalPol s w = numCTostr (evalPolAux2 s w)
	
	termToNumC :: Term ->NumC
	termToNumC (R(a,b)) = (a,0.0)
	termToNumC (C ((a,b),c)) = (a,b)
