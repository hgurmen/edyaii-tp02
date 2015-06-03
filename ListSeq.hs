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
filterL f (x:xs) = let (hd, tl) = f x ||| filterL f xs
                   in if hd then x : tl
                      else tl


mapL :: (a -> b) -> [a] -> [b]
mapL f [] = []
mapL f (x:xs) = let (hd, tl) = f x ||| mapL f xs
                in hd : tl


showtL :: [a] -> TreeView a [a]
showtL []  = EMPTY
showtL [x] = ELT x
showtL s = let len = Prelude.length s
               m = div len 2
               (l, r) = splitAt m s
           in NODE l r


showlL :: [a] -> ListView a [a]
showlL []     = NIL
showlL (x:xs) = CONS x xs


contract :: (a -> a -> a) -> [a] -> [a]
contract f [] = []
contract f [x] = [x]
contract f (x:y:xs) = let (hd, tl) = f x y ||| contract f xs
                      in hd : tl


reduceL :: (a -> a -> a) -> a -> [a] -> a
reduceL f b [] = b
reduceL f b [x] = f b x
reduceL f b s = reduceL f b (contract f s)


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

-- combine f s s' IndiceActualEsPar   ----->> 
-- combine es O(f) en cada indice impar.
combine :: (a -> a -> a) -> [a] -> [a] -> Bool -> [a]
combine _ []       _           _     = []
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





