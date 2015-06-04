module ArrSeq where

import Seq
import Par
import Arr

isEven :: Integral a => a -> Bool
isEven x
    | mod x 2 == 0 = True
    | otherwise = False
    
singletonA :: a -> Arr a
singletonA x = Arr.fromList [x]

appendA :: Arr a -> Arr a -> Arr a
appendA s s' = flatten (Arr.fromList [s, s'])

takeA :: Arr a -> Int -> Arr a
takeA s n = subArray 0 n s

dropA :: Arr a -> Int -> Arr a
dropA s n = subArray n (len - n) s
    where len = Arr.length s

mapA :: (a -> b) -> Arr a -> Arr b
mapA f s = tabulate (\i -> f (s!i)) len
    where len = Arr.length s

filterA :: (a -> Bool) -> Arr a -> Arr a
filterA f s = flatten (tabulate (\i -> if f (s!i) then singletonA (s!i) else empty) len)
    where len = Arr.length s

showtA :: Arr a -> TreeView a (Arr a)
showtA s 
    | len == 0 = EMPTY
    | len == 1 = ELT (s!0)
    | otherwise = NODE (takeA s m) (dropA s m)
    where len = Arr.length s
          m = div len 2

showlA :: Arr a -> ListView a (Arr a)
showlA s
    | len == 0 = NIL
    | otherwise = CONS (s!0) (dropA s 1)
    where len = Arr.length s

-- Contrae la secuencia con la operación binaria.
contract :: (a -> a -> a) -> Arr a -> Arr a
contract f s
    | isEven len = tabulate even_tab m
    | otherwise = tabulate odd_tab (1 + m)
    where len = Arr.length s
          m = div len 2
          even_tab i = f (s!(2*i)) (s!(2*i + 1))
          odd_tab i = if 2*i == (len - 1)
                      then s!(2*i)
                      else f (s!(2*i)) (s!(2*i + 1))

reduceA :: (a -> a -> a) -> a -> Arr a -> a
reduceA f b s
    | len == 0 = b
    | len == 1 = f b (s!0)
    | otherwise = reduceA f b (contract f s)
    where len = Arr.length s

-- Combina las dos secuencias dadas con la operación binaria.
combine :: (a -> a -> a) -> Arr a -> Arr a -> Arr a
combine f s s' = tabulate fun len
    where len = Arr.length s
          fun i = let m = div i 2
                  in if isEven i
                     then s'!m
                     else f (s'!m) (s!(i - 1))

scanA :: (a -> a -> a) -> a -> Arr a -> (Arr a, a)
scanA f b s
    | len == 0 = (empty, b)
    | len == 1 = (singletonA b, f b (s!0))
    | otherwise = let (s', rdc) = scanA f b (contract f s)
                  in (combine f s s', rdc)
    where len = Arr.length s

instance Seq Arr where
    emptyS = empty
    singletonS = singletonA
    lengthS = Arr.length
    nthS = (!)  
    tabulateS = tabulate
    mapS = mapA
    filterS = filterA
    appendS = appendA
    takeS = takeA
    dropS = dropA
    showtS = showtA
    showlS = showlA
    joinS = flatten
    reduceS = reduceA
    scanS = scanA
    fromList = Arr.fromList
