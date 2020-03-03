module Tarea02 where
    
    import Data.List
    
    --Raiz cuadrada de un entero
    intSqrt :: Int -> Int
    intSqrt = ceiling . sqrt . fromIntegral
    
    --Conjunto de potencias de enteros menores/iugales a n.
    pwrs :: Int -> [Int]
    pwrs n = [2^x | x<-[0..n]]
    
    --Regresa la potencia de 2 mas grande menor que x.
    gtrPower2 :: Int -> Int
    gtrPower2 x = last (takeWhile (<x) (pwrs (intSqrt x)))
    
    --Determina si una lista tiene unico elemento repetido
    hasJustOne :: (Eq a) => [a] -> Bool
    hasJustOne xs = length (nub xs) == 1
    
    --Regresa una lista de los segmentos en una lista
    segs :: (Eq a) => [a] -> [[a]]
    segs = concat . map inits.tails
    
    --Regresa el mayor numero de apariciones consecutivas del mismo elemento
    inarow :: (Eq a) => [a] -> Int
    inarow l = maximum (map length singles)
        where singles = filter (\x-> hasJustOne x) (segs l)

    -- Regresa una lista de numeros menores/iguales a un parametro dado que cumplan la propiedad a^3+b^3=c^3+d^3 
    ramanujan :: Int -> [(Int, Int, Int, Int)]
    --Optimización
    ramanujan n = [(a,b,c,d)| a<-[1..n], b<-[(a+1)..n], c<-[(a+1)..n], d<-[(c+1)..n], a^3+b^3==c^3+d^3]
    --Implementacion que elimina las tuplas duplicadas es correcta pero ineficiente
    --ramanujan n = nubBy sameNumbers [(a,b,c,d)| a<-[1..n], b<-[1..n], c<-[1..n], d<-[1..n], isRamanujan a b c d]
    --Implementacion que genera tuplas que contienen los mismos numero (a,b,c,d) y (a,c,d,b) por ejemplo
    --ramanujan n = [(a,b,c,d)| a<-[1..n], b<-[1..n], c<-[1..n], d<-[1..n], isRamanujan a b c d]
    
    --Determina si dos tuplas de 4 tienen los mismos numeros
    sameNumbers :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Bool
    sameNumbers (a,b,c,d) (e,f,g,h) = length (union (a:b:c:d:[]) (e:f:g:h:[])) == 4
    
    -- Determina si cuatro numeros dados cumplen con la propiedad a^3+b^3=c^3+d^3 y son distintos entre si
    isRamanujan :: Int -> Int -> Int -> Int -> Bool
    isRamanujan a b c d = (a^3+b^3==c^3+d^3) && a/=b && a/=c && a/=d && b/=c && b/=d && c/=d
