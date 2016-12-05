module QuadTree where

data QT a = C a | Q (QT a) (QT a) (QT a) (QT a)
    deriving (Eq, Show)


data Mat a = Mat {
    nexp :: Int,
    mat :: QT a
    }
    deriving (Eq, Show)


--funzione che trasforma un quadTree in una lista che contiene tutti i suoi nodi
toList :: QT a -> [a]
toList (C x)           = x:[]
toList (Q q1 q2 q3 q4) = (toList q1) ++ (toList q2) ++ (toList q3) ++ (toList q4)

--funzione che, data una matrice m e un intero n, ritorna la colonna n-esima in forma di lista
getColonne :: Mat a -> Int -> [a]
getColonne m n 
   | (n<((2^(nexp m))*(2^(nexp m)))) = ((toList(mat m))!!n)  : getColonne m (n +(2^(nexp m)))
   | otherwise = []


--moltiplicazione tra i vettori (x:xs) e (y:ys) della stessa lunghezza; ritorna uno scalare
mul :: Num a => [a] -> [a] -> a
mul [] [] = 0
mul (x:xs) (y:ys) 
    | (length (x:xs)) /= (length (y:ys)) = error "Attenzione!! I due vettori devono essere della stessa lunghezza."
    | otherwise = x*y + (mul xs ys)




--funzione che risolve il problema
--fa la moltiplicazione tra v, m, v trasposto, con v lista di interi e m matrice
f :: Num a => [a] -> Mat a -> a
f v m 
    | ((2^(nexp m)) /= (length v)) = error "Attenzione!! il vettore e la matrice devono avere la sressa lunghezza,"
    | otherwise = mul (f1 v m 0) v
        where
	    f1 v m i 
                | (i<(2^(nexp m))) = (mul v (getColonne m i)) : (f1 v m (i+1))
                | otherwise = []
