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
    foldRev = foldr (\x tail -> tail ++ [x]) []
    
    --filter
    foldFltr :: (a -> Bool) -> [a] -> [a]
    foldFltr f = foldr (\x rec -> if f x then (x:rec) else rec) []
    
    --inits
    foldInits :: [a] -> [[a]]
    foldInits xs = foldr (\_ rec -> [init (head rec)] ++ rec) [xs] xs
    
    --tails
    foldTails :: [a] -> [[a]]
    foldTails = foldr (\x rec -> ([x] ++ (head rec)):rec) [[]] 
