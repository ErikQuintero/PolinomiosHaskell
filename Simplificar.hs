module Simplificar where
	import Validar
	
	type Pol = [TermC2]
	type TermC2 = (Int, (Double,Double))
    
	--Funcion que combierte todos los elementos de un polinomio a Terminos complejos
	simpList1 :: Polinomio -> Polinomio	
	simpList1 [] = []
	simpList1 (x:xs) = [(termRToCAux x)] ++ simpList1 xs  
	
	--Funcion que transforma real a un termino comlejo
	termRToCAux :: Term -> Term 
	termRToCAux (R (a,b)) = (C ((0.0,a), b))
	termRToCAux (C ((a,b), c)) = (C ((a,b), c))
	
	--Funcion que transforma un termino a un termC2
	termTOTermC2 :: Term ->TermC2
	termTOTermC2 (R (a,b)) = (b,(a,0))
	termTOTermC2 (C ((a,b),c)) = (c,(a,b))
	
	--Funcion que transforma un termC2 a un termino
	termC2TOTermC :: TermC2 ->Term
	termC2TOTermC (c,(a,b)) = C ((a,b),c)
	
	--Funcion que tranforma un polinomio a un pol
	polinomioToPol :: Polinomio ->Pol
	polinomioToPol [] = []
	polinomioToPol (x:xs) = [(termTOTermC2 x)] ++ (polinomioToPol xs)
	
	----Funcion que tranforma un pol a un polinomio 
	polToPolinomio :: Pol ->Polinomio
	polToPolinomio [] = []
	polToPolinomio (x:xs) = [(termC2TOTermC x)] ++ (polToPolinomio xs)
	
	--Funcion que suma dos terminos complejos
	sumaTC :: Term -> Term -> Term
	sumaTC (C ((a,b), c)) (C ((x,y), z)) = if (not (c == z)) then error "no se puede sumar" else C ((a+x,b+y), c)

        --Funcion auxiliar para la funcion msort    
        merge :: Ord a => [a] -> [a] -> [a]
        merge xs [] = xs
        merge [] ys = ys
        merge (x:xs) (y:ys) | x <= y    = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys
        
        --Funcion que ordena una lista en tiempo (n (log n)) 
        msort :: Ord a => [a] -> [a]
        msort [] = []
        msort [a] = [a]
        msort xs = merge (msort (firstHalf xs)) (msort (secondHalf xs))
        firstHalf  xs = let { n = length xs } in take (div n 2) xs
        secondHalf xs = let { n = length xs } in drop (div n 2) xs
        
        --Funcion que simplifica un polinomio
        simplificar :: Polinomio ->Polinomio
        simplificar [] = []
        simplificar [(C((a,b),c))] = [(C((a,b),c))]
        simplificar (x:y:xs) = if ((getEx x) == (getEx y)) then simplificar ([(sumaTC x y)] ++ xs) else [x] ++ (simplificar (y:xs))
        
        --Funcion que tranforma la cadena inicial en algo que haskell entienda
        strToHaskell :: String ->Polinomio
        strToHaskell a = simplificar (polToPolinomio (msort (polinomioToPol (simpList1 (listToPol a))))) 
        
        
