module ListSeq where

import Par
import Seq

tabulateL :: (Int -> a) -> Int -> [a]
tabulateL f 0 = []
tabulateL f n = (f 0) : (tabulateL (f . (+1)) (n - 1))

singletonL :: a -> [a]
singletonL x = [x]

takeL :: [a] -> Int -> [a]
takeL xs n = Prelude.take n xs

dropL :: [a] -> Int -> [a]
dropL xs n = Prelude.drop n xs

filterL :: (a -> Bool) -> [a] -> [a]
filterL f [] = []
filterL f (x:xs) = let (hd, tl) = f x ||| filterL f xs
                   in if hd
                      then x : tl
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

-- Contrae la secuencia con la operación binaria.
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
scanL f b []  = ([], b)
scanL f b [x] = ([b], f b x)
scanL f b s  = let (s', rdc) = scanL f b (contract f s)
               in (combine f s s' True, rdc)

-- Combina las dos secuencias dadas con la operación binaria.
combine :: (a -> a -> a) -> [a] -> [a] -> Bool -> [a]
combine _ []       _           _     = []
combine f s@(x:xs) s'@(x':xs') True  = x' : (combine f s s' False)
combine f [x]      (x':xs')    False = []
combine f (x:_:xs) (x':xs')    False = let (hd, tl) = (f x' x) ||| (combine f xs xs' True)
                                       in hd : tl

instance Seq [] where
    emptyS     = []
    singletonS = singletonL
    lengthS    = length
    nthS       = (!!)
    tabulateS  = tabulateL
    mapS       = mapL
    filterS    = filterL
    appendS    = (++)
    takeS      = takeL
    dropS      = dropL
    showtS     = showtL
    showlS     = showlL
    joinS      = concat
    reduceS    = reduceL
    scanS      = scanL
    fromList   = id
