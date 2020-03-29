module Simplificar where
	import Validar

	simpList1 :: Polinomio -> Polinomio	
	simpList1 [] = []
	simpList1 (x:xs) = [(termRToCAux x)] ++ simpList1 xs  
	
	termRToCAux :: Term -> Term 
	termRToCAux (R (a,b)) = (C ((0.0,a), b))
	termRToCAux (C ((a,b), c)) = (C ((a,b), c))
	
	sumaTC :: TermC -> TermC -> TermC
	sumaTC ((a,b), c) ((x,y), z) = if (not (c == z)) then error "no se puede sumar" else ((a+x,b+y), c)
	
	
