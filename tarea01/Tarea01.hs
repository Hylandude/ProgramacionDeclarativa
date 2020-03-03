module Tarea1 where
    
    import Data.Char
    import Data.List
    import Data.Maybe
    
    data Color = Rojo | Amarillo | Verde | Azul deriving (Eq , Show, Enum)
    data Balcanes = Albania | 
                    Bulgaria |
                    BosniayHerzegovina |
                    Kosovo |
                    Macedonia |
                    Montenegro deriving (Eq, Show)
    
    type Ady = [(Balcanes, Balcanes)]
    type Coloracion = [(Color, Balcanes)]
    
    adyacencias :: Ady
    adyacencias =  [(Albania, Montenegro),
                    (Albania, Kosovo),
                    (Albania, Macedonia),
                    (Bulgaria, Macedonia),
                    (BosniayHerzegovina, Montenegro),
                    (Kosovo, Macedonia),
                    (Kosovo, Montenegro)]
    
    --Se agrego Enum a color para poder hacer esta lista sin tener que escribirlos explicitamente
    allColors :: [Color]
    allColors = [Rojo ..]
    
    --Elimina los espacios al principio y al final de una cadena
    trim :: String -> String
    trim = dropWhileEnd isSpace . dropWhile isSpace
    
    --Elimina las mayusculas de una cadena
    quitaMayusculas :: String -> String
    quitaMayusculas str = trim [ c | c <- str, not (isUpper c) ]
    
    --Elimina los caracteres que no sean letras en una cadena
    soloLetras :: String -> String
    soloLetras str = trim [ c | c <- str, ((isUpper c) || (isLower c))]
    
    --Determina si una cadena es prefijo de otra
    prefijo :: String -> String -> Bool
    prefijo prefix input    = (take (length prefix) input) == prefix
    --Definición clasica
    --prefijo "" _          = True
    --prefijo (p:ps) (i:is) = if i == p then prefijo ps is else False
    --Solucion de 3 palabras
    --prefijo prefix input  = isPrefixOf prefix input
    
    --Divide una lista en dos
    parte :: [a] -> ([a] , [a])
    parte xs = splitAt (div (length xs) 2) xs
    
    --Dadas dos listas las une de manera ordenada de menor a mayor
    mezcla :: (Ord a) => [a] -> [a] -> [a]
    mezcla [] b          = b
    mezcla a []          = a
    mezcla (a:as) (b:bs) = if a < b then a:(mezcla as (b:bs)) else b:(mezcla (a:as) bs)
    
    --Ordena listas usando el algoritmo mergeSort (orden standard de menor a mayor)
    mergeSort :: (Ord a) => [a] -> [a]
    mergeSort []    = []
    mergeSort [x]   = [x]
    mergeSort xs    = mezcla (mergeSort f) (mergeSort s)
        where (f, s) = parte xs
              
    --Dadas dos listas las une de manera ordenada siguiende una comparacion explicita que se pasa como argumento
    mezclaCon :: (Ord a) => (a-> a-> Ordering) -> [a] -> [a] -> [a]
    mezclaCon _ [] b             = b
    mezclaCon _ a []             = a
    mezclaCon comp (a:as) (b:bs) = if (comp a b) == LT then a:(mezclaCon comp as (b:bs)) else b:(mezclaCon comp (a:as) bs)
    
    --Ordena una lista usando la comparacion explicita que se pasa como argumento
    mergeSortCon :: (Ord a) => (a-> a-> Ordering) -> [a] -> [a]
    mergeSortCon _ []    = []
    mergeSortCon _ [x]   = [x]
    mergeSortCon comp xs = mezclaCon comp (mergeSortCon comp f) (mergeSortCon comp s)
        where (f, s) = parte xs
        
    --Obitiene el color de un Balcan en una Coloracion
    getColor :: Balcanes -> Coloracion -> Maybe(Color)
    getColor b l
        | length colors >= 1 = Just (head colors)
        | otherwise          = Nothing
        where colors = [c | (c,p)<-l, p==b]
              
    --Determina si dos colores son iguales. Se usa el wrapper Maybe para cachar un Nothing, esto define que no tiene color
    compareColor :: Maybe(Color) -> Maybe(Color) -> Bool
    compareColor Nothing _         = False
    compareColor _ Nothing         = False
    compareColor (Just a) (Just b) = a == b
    
    --Obtiene todos los paises en una matriz de adyacencia
    getPaises :: Ady -> [Balcanes]
    getPaises a = nub ([p | (p,_)<-a] ++ [b | (_,b)<-a])
        
    --Determina si la coloracion dada es buena respecto a la matriz de adyacencias.
    --Una coloracion es buena si para cada par de paises adyacentes su color es distinto
    esBuena :: Ady -> Coloracion -> Bool
    esBuena ady col = notElem True (map (\(x,y)-> compareColor (getColor x col) (getColor y col)) ady)
    
    --Determina si la coloracion dada es completa respecto a la matriz de adyacencias.
    --Una coloracion es completa si todos los paises de la matriz estan coloreados
    esCompleta :: Ady -> Coloracion -> Bool
    esCompleta ady col = notElem Nothing (map (\x-> getColor x col) (getPaises ady))
    
    --Determina si la coloracion dad es completa Y buena.
    esBuenota :: Ady -> Coloracion -> Bool
    esBuenota ady col = (esBuena ady col) && (esCompleta ady col)
    
    --Calcula todas las coloraciones buenas y completas respecto a la matriz de adyacencias recibida.
    coloraciones :: Ady -> [Coloracion]
    coloraciones a = filter (\c-> esBuenota a c) allColorations
        where allPairs       = [(y, x)| x<-(getPaises a), y<-allColors]
              allColorations = map (\p -> nubBy (\(x,_) (y,_) -> x==y) p) (permutations allPairs)
