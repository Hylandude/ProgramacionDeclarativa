module Tarea03 where
    
    import Data.List
    
    --Funciones escritas como instancias de foldr
    --concat
    foldConcat :: [[a]] -> [a]
    foldConcat = foldr (++) [] 
    
    --minimum
    foldMin :: (Ord a) => [a] -> a
    foldMin xs = foldr (\x y-> if x < y then x else y) (head xs) xs
    
    --reverse
    foldRev :: [a] -> [a]
    foldRev = foldr (\x rec -> rec ++ [x]) []
    
    --filter
    foldFltr :: (a -> Bool) -> [a] -> [a]
    foldFltr f = foldr (\x rec -> if f x then (x:rec) else rec) []
    
    --inits
    foldInits :: [a] -> [[a]]
    foldInits xs = foldr (\_ rec -> [init (head rec)] ++ rec) [xs] xs
    
    --tails
    foldTails :: [a] -> [[a]]
    foldTails = foldr (\x rec -> ([x] ++ (head rec)):rec) [[]]
    
    --Operador de plegado para Int
    foldi :: (a -> a) -> a -> Int -> a
    foldi f q 0 = q
    foldi f q i = f (foldi f q (pred i))
    
    --Suma como instancia de foldi (a+b)
    foldAdd :: Int -> Int -> Int
    foldAdd a b = foldi (\x -> succ(x)) a b
    
    --Multiplicacion como instancia de foldi (a*b)
    foldMul :: Int -> Int -> Int
    foldMul a b = foldi (\x -> x+a) 0 b

    --Exponente como instancia de foldi (a^b)
    foldExpt :: Int -> Int -> Int
    foldExpt a b = foldi (\x -> x*a) 1 b
    
    --Suma de los primeros n naturales al cuadrado
    sumq :: Int -> Int
    sumq n = foldl (\x y -> x + y^2) 0 [0..(n-1)]
    
    --Elimina los todos los elementos de la segunda lista que aparecian en la primera.
    remove :: (Eq a) => [a] -> [a] -> [a]
    remove xs ys = foldr (\x rec -> if elem x xs then rec else (x:rec)) [] ys
    
    --Elimina los elementos adyacentes duplicados en una lista
    remdups :: (Eq a)=> [a] -> [a]
    remdups l = foldr (\x rec -> if x==(head rec) then rec else (x:rec)) [(last l)] l
    
