module ListSeq where

import Debug.Trace
import Par
import Seq

-- *********************** NOTA ********************************
-- Para crear un array con una lista:
-- Seq.fromList [1,2,3,4] :: [Int]

tabulateL :: (Int -> a) -> Int -> [a]
tabulateL f 0 = []
tabulateL f n = (f 0) : (tabulateL (f . (+1)) (n - 1))

takeL :: [a] -> Int -> [a]
takeL xs n = Prelude.take n xs

dropL :: [a] -> Int -> [a]
dropL xs n = Prelude.drop n xs

filterL :: (a -> Bool) -> [a] -> [a]
filterL f [] = []
filterL f (x:xs) = let (hd, tl) = f x ||| filterL f tl
                   in if hd then x : filterL f tl
                      else filterL f tl

mapL :: (a -> b) -> [a] -> [b]
mapL f [] = []
mapL f (x:xs) = let (hd, tl) = fx ||| mapL f tl
                in hd : tl

-- PREGUNTAR: paralelizar el take y el drop en este caso no hace niguna mejora
-- con respecto al trabajo y/o profundidad, verdad?
-- Simplemente llevaria de O(n) a O(n/2) que es lo mismo...
showtL :: [a] -> TreeView a [a]
showtL []  = EMPTY
showtL [x] = ELT x
showtL s  = let len = Prelude.length s
                m = div len 2
                (l, r) = takeL s m ||| dropL s m
            in NODE l r

-- W = 1, S = 1
showlL :: [a] -> ListView a [a]
showlL []     = NIL
showlL (x:xs) = CONS x xs

-- PREGUNTAR: Hay que paralelizar el cálculo de f x y junto con el del paso recursive de reduceStep????
-- Aún cuando paralelizás esto tendría costo lineal, no?
contract :: (a -> a -> a) -> [a] -> [a]
contract f []       = []
contract f [x]      = [x]
contract f (x:y:xs) = f x y : contract f xs
--contract f (x:y:xs) = let (fun, resto) = f x y ||| contract f xs
--                      in fun : resto

{-
versión paralela de contract
El trabajo de cntrct es sum de i=0 a |xs|-1 de W(f x y)

La profundidad sería:
|xs| + máximo de todas las aplicaciones en el árbol de reducción de S(f x y)
-}

cntrct :: (a -> a -> a) -> [a] -> [a]
cntrct f [] = []
cntrct f [x] = [x]
cntrct f (x:y:xs) = let (hd, tl) = f x y ||| cntrct f xs
                    in hd : tl

-- W = n + (sumatoria de 0 a log2 n f)
{-
reduceL se va aplicando recursivamente con una secuencia con la mitad de
tamaño hasta llegar a lista de 1 elemento (logarítmico)

-}

reduceL :: (a -> a -> a) -> a -> [a] -> a
reduceL f b [] = b
reduceL f b [x] = f b x
reduceL f b s = reduceL f b (contract f s)

{- --para debuggear
let asd = contract f xs
                  in trace (show xs ++ " -> " ++ show asd) reduceS f b asd
-}

scanL :: (a -> a -> a) -> a -> [a] -> ([a], a)
scanL f b []  = ([], b) -- da como resultado ([], b)
scanL f b [x] = ([b], f b x)
scanL f b s  = let (s', rdc) = scanL f b (contract f s)
               in (combine f s s' True, rdc)

{-
Ejemplo de combin:

combin (+) [1,2,3,4,5] [0,3,10] True =
0: (combin (+) [1,2,3,4,5] [0,3,10] False) =
0:(0+1): (combin (+) [3,4,5] [3,10] True) =
0:1:3: (combin (+) [3,4,5] [3,10] False) =
0:1:3:(3+3): (combin (+) [5] [10] True) =
0:1:3:6:10: (combin (+) [5] [10] False) =
0:1:3:6:10:[]


Cómo funciona combine:
Toma:
    Una función f :: a -> a -> a
    Una secuencia s, la secuencia original del scanS.
    Una secuencia s', la secuencia devuelta por la aplicación recursiva de scanS
        sobre la lista contraída de s.
    Un valor Booleano que indica con True si el índice actual de la lista a
        retornar es par.

Funciona:
    Cuando la lista original resulta [], devuelve []
    Cuando el booleano es False y la lista tiene exactamente un elemento devuelve
        lista vacía también. Esto es porque actualmente tengo que construír un
        índice impar de la lista resultante, y cuando queda un sólo elemento es
        porque no voy a poder construír el próximo.
    Cuando el booleano es True y la lista no es vacía, pongo el primer elemento
        de la lista s'.
    Es orden lineal en la longitud de la primer lista (la original)
-}

-- combine f s s' IndiceActualEsPar   ----->> algo
combine :: (a -> a -> a) -> [a] -> [a] -> Bool -> [a]
combine _ []       _           _ = []
-- PREGUNTAR: esto creo que no hace falta...
--combine _ _        []          _ = []
combine f s@(x:xs) s'@(x':xs') True  = x' : (combine f s s' False) -- O(1)
combine f [x]      (x':xs')    False = []
combine f (x:_:xs) (x':xs')    False = let (hd, tl) = (f x' x) ||| (combine f xs xs' True) -- O(f)
                                       in hd : tl

instance Seq [] where
    emptyS     = []          --
    singletonS = singletonS
    lengthS    = length      --
    nthS       = (!!)        --
    tabulateS  = tabulateL
    mapS       = mapL
    filterS    = filterL
    appendS    = (++)        --
    takeS      = takeL
    dropS      = dropL
    showtS     = showtL
    showlS     = showlL
    joinS      = concat      --
    reduceS    = reduceL
    scanS      = scanL
    fromList   = id          --





