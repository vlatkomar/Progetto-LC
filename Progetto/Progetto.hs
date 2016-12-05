module Progetto where

data QT a = C a | Q (QT a) (QT a) (QT a) (QT a)
    deriving (Eq, Show)


data Mat a = Mat {
    nexp :: Int,
    mat :: QT a
    }
    deriving (Eq, Show)


r (C x)          = x:[]
r (Q q1 q2 q3 q4)= (r q1) ++ (r q2) ++ (r q3) ++ (r q4)

getColonne m n 
   | (n<((nexp m)*(nexp m))) = ((r(mat m))!!n)  : getColonne m (n +(nexp m))
   | otherwise = []

f1 v m i 
    | (i<(nexp m)) = (mul v (getColonne m i)) : (f1 v m (i+1))
    | otherwise = []

--moltiplicazione tra i vettori (x:xs) e (y:ys) della stessa lunghezza
mul [] [] = 0
mul (x:xs) (y:ys) 
    | (length (x:xs)) /= (length (y:ys)) = error "Attenzione!! I due vettori devono essere della stessa lunghezza."
    | otherwise = x*y + (mul xs ys)

--funzione che risolve il problema
--fa la moltiplicazione tra v, m, v trasposto, con v lista di interi e a matrice
f v m 
    | ((nexp m) /= (length v)) = error "Attenzione!! il vettore e la matrice devono avere la sressa lunghezza,"
    | otherwise = mul (f1 v m 0) v



--------------------------------------
u  = C 1
v1 = Q (C 7) (C 9) (C 3) (C 2)
v2 = Q (C 2) (C 2) (C 2) (C 2) 
v3 = Q (C 5) (C 4) (C 2) (C 4) 
v4 = Q (C 3) (C 0) (C 3) (C 3)
z  = C 0
mt = Mat 4 (Q v1 v2 v3 v4)
