import Data.List

type TermR = (Double,Int)
type TermC = ((Double,Double) , Int)
data Term = R TermR | C TermC
type Polinomio = [Term]

instance Show Term where 
        show e = case e of
         (R x) -> "(" ++ (show (fst x)) ++  "," ++ (show (snd x)) ++ ")"
         (C y) -> "(" ++ "(" ++ (show (fst(fst y))) ++ "," ++ (show (snd(fst y))) ++ ")" ++ "," ++ (show (snd y)) ++ ")"  

--Verifica si un numero es valido
numVAux :: String -> Bool
numVAux s = if elem (read s) [x | x <- [0.0..1000000.0]] then True else False

--Auxiliar Validar
numLaStrL :: [Double] -> [String]
numLaStrL [] = []
numLaStrL (x:xs) = [(show x)] ++ (numLaStrL xs)

--Verifica si un termino es valido auxiliar
numV :: String -> Bool
numV s = if (elem s (numLaStrL ([x | x <- [0.0..1000000.0]]))) then True else False

--Valida polinomio completo
numVPol :: [String] -> Bool
numVPol [] = True
numVPol (x:xs) = if ((valTerm x) == False) then False else (numVPol xs) 

--Elimina las subcadenas de los terminos de los polinomios 
elimC :: String -> String
elimC "" = ""
elimC (x:xs) = if x == ',' then "" else [x] ++ elimC xs

--Mete los terminos a una lista
listAux :: String -> [String]
listAux "" = []
listAux (x:xs) = [elimC (x:xs)] ++ listAux (drop (length (elimC (x:xs))) xs)

--Devuelve el coeficiente de un termino como cadena
strToCAux :: String -> String
strToCAux "" = ""
strToCAux (x:xs) = if x == '^' then "" else [x] ++ (strToCAux xs)

--Devuelve convierte el coeficiente a double 
strToCr :: String -> Double
strToCr s = if (strToCAux s) == "x" then read "1" :: Double else read (delete 'x' (strToCAux s)) :: Double

--Auxiliar para los negativos en la parte real del coeficiente
strToCcrMAux :: String -> Bool
strToCcrMAux (x:xs) = if (head xs) == '-' then True else False 

--Devuelve la parte real del coeficiente si es positiva
strToCcrFAux :: String -> String
strToCcrFAux "" = ""
strToCcrFAux (x:xs) = if (x == '+') || (x == '-') then "" else delete '(' [x] ++ (strToCcrFAux xs)

--Devuelve la parte real del coeficiente si es negativa
strToCcrTAux :: String -> String
strToCcrTAux s = "-" ++ (strToCcrFAux (drop 2 s))

--Devuelve la parte real del coeficiente como cadena
strToCcrAux :: String -> String
strToCcrAux s = if (strToCcrMAux s == True) then strToCcrTAux s else strToCcrFAux s

--Devuelve la parte real del coeficiente
strToCcr :: String -> Double
strToCcr s = read (strToCcrAux s) :: Double

--Verifica si la parte imaginaria es negativa
strToCccMAux :: String -> Bool
strToCccMAux s = if head (((drop ((length (strToCcrAux s)) + 1) s))) == '-' then True else False

--Devuelve la parte imaginaria del coeficiente si es positiva
strToCccFAux :: String -> String
strToCccFAux "" = ""
strToCccFAux s = delete 'x' (delete 'i' ((delete ')' (strToCAux (drop ((length (strToCcrAux s)) + 2) s)))))   

--Devuelve la parte imaginaria del coeficiente si es negativa
strToCccTAux :: String -> String
strToCccTAux s = "-" ++ (strToCccFAux s)

--Devuelve la parte imaginaria del coeficiente como cadena
strToCccAux :: String -> String
strToCccAux s = if (strToCccMAux s) == True then (strToCccTAux s) else (strToCccFAux s)

--Devuelve la parte imaginaria del coeficiente
strToCcc :: String -> Double
strToCcc s = read (strToCccAux s) :: Double 

--Devuelve el exponente de un termino
strToEx :: String -> Int
strToEx s = if (last s) == 'x' then 0 else read (drop 1 (drop (length (strToCAux s)) s)) :: Int

--Tranforma un String a un termino real
strToTermR :: String -> TermR
strToTermR s = if (not (elem 'x' s)) then (read s :: Double , 0) else (strToCr s , strToEx s)

--Tranforma un String a un termino complejo auxiliar
strToTermCM :: String -> TermC
strToTermCM s = if (not (elem 'x' s)) && (elem 'i' s) then ((0.0 , strToCcr (delete 'i' s)) , 0) else ((strToCcr s , strToCcc s) , strToEx s)  

--Tranforma un string a un termino complejo 
strToTermC :: String -> TermC
strToTermC s = if (elem 'x' s) && (elem 'i' s) && (not(elem '(' s)) then ((0.0 , (strToCcr (delete 'x' (delete 'i' (strToCAux s))))), strToEx s) else strToTermCM s 

--Auxiliar que tranforma una lista de cadenas a un polinomio 
listToPolAux :: [String] -> Polinomio
listToPolAux [] = []
listToPolAux (x:xs) = if (elem 'i' x) then [C (strToTermC x)] ++ (listToPolAux xs) else  [R (strToTermR x)] ++ (listToPolAux xs)

--Tranforma una cadena a un polinomio
listToPolAux2 :: String -> Polinomio
listToPolAux2 "" = []
listToPolAux2 s = if (not (elem (delete 'x' (delete '^' (delete 'i' (delete '(' (delete ')' (delete '+' (delete '-' (delete '-' s)))))))) ((numLaStrL ([x | x <- [0.0..1000000.0]]))))) then error "Polinomio invalido" else listToPolAux (listAux s)

--Devuelve el exponente de un termino
getEx :: Term -> Int
getEx (R x) = (snd x)
getEx (C y) = (snd y)
  
--Valida Exponente
valEx :: Polinomio -> Bool 
valEx [] = True
valEx (x:xs) = if ((getEx x) < 0) then False else valEx xs

--Valida un String
valTerm :: String -> Bool
valTerm s = numV (delete '(' (delete ')' (delete 'i' (delete '+' (delete '-' (delete '-' (delete '^' (delete 'x' s)))))))) 

--Valido todo el termino
valTotal :: String -> Bool
valTotal s = (valEx (listToPolAux2 s)) && (numVPol (listAux s)) && (valXEx s)

--Busca si un polinomio contiene la subcadena x^
valXExAux :: String -> Bool
valXExAux "" = False
valXExAux (x:xs)
    | (x == 'x') = if ((head xs) == '^') then True else False
    | otherwise = valXExAux xs
    
--Valida si un esponente es valido en cuanto a x
valXEx :: String -> Bool
valXEx s
    | (elem 'x' s) && (not (elem '^' s)) = True 
    | (elem 'x' s) = (valXExAux s)
    | otherwise = if (elem '^' s) then False else True      

--Transforma String a polinomio validando exponentes
listToPol :: String -> Polinomio
listToPol s = if ((valTotal s) == False) then error "Polinomio invalido" else (listToPolAux2 s)
