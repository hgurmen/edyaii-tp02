module ArrSeq where

-- *********************** NOTA ********************************
-- Para crear un array con una lista:
-- Seq.fromList [1,2,3,4] :: Arr Int

import Seq
import Par
import Arr
import Debug.Trace

isEven :: Integral a => a -> Bool
isEven x
    | mod x 2 == 0 = True
    | otherwise = False

singletonA :: a -> Arr a
singletonA x = Arr.fromList [x]

-- Preguntar: cual es más efectivo??
appendA :: Arr a -> Arr a -> Arr a
appendA s s' = flatten (Arr.fromList [s, s'])

-- La función que aplico a tabulate es O(1) luego la profundidad
-- es O(1) y el trabajo es O(|s| + |s'|) pues recorro todo
_appendA :: Arr a -> Arr a -> Arr a
_appendA s s' = tabulate (\i -> if i < ls then s!i else s'!(i - ls)) total
    where ls = Arr.length s
          ls' = Arr.length s'
          total = ls + ls'
          
takeA :: Arr a -> Int -> Arr a
takeA s n = subArray 0 n s

dropA :: Arr a -> Int -> Arr a
dropA s n = subArray n (len - n) s
    where len = Arr.length s

mapA :: (a -> b) -> Arr a -> Arr b
mapA f s = tabulate (\i -> f (s!i)) len
    where len = Arr.length s


-- filterA no tiene buen costo ya que usa append y en el caso de trabajo,
-- append tiene trabajo O(|s| + |s'|) y eso implica que quede trabajo cuadrático
-- SOLO en el append del final...
filterA :: (a -> Bool) -> Arr a -> Arr a
filterA f s
    | len == 0 = empty
    | len == 1 = if f (s!0) then singletonA (s!0)
                 else empty
    | otherwise = let (l, r) = filterA f (takeA s m)
                               |||
                               filterA f (dropA s m)
                  in appendA l r
    where len = Arr.length s
          m = div len 2
          
-- Este rinde mas, hace un flatten que no tengo ni idea cual es el costo (aparentemente
-- tiene W O(|s|) y S O(lg |s|) donde s es la secuencia de secuencias.
fltrA :: (a -> Bool) -> Arr a -> Arr a
fltrA f s = flatten (tabulate (\i -> if f (s!i) then singletonA (s!i) else empty) len)
    where len = Arr.length s
    
-- PREGUNTAR: join se tiene que implementar con flatten o hay que hacerla nosotros?
-- una idea de hacer join es usando tabulate y reduce, capaz
-- PREGUNTAR: aparentemente queda cuadrático
{-
ejemplo:
tengo
<a1,a2,a3,a4>
con longitudes a,b,c,d respectivamente

reduce que hace primero:
append de a1 + a2 => O(a + b)
append de a3 + a4 => O(c + d)
append de (a1 + a2) + (a3 + a4) = O(a + b + c + d)
-}
joinA :: Arr (Arr a) -> Arr a
joinA s = reduceA fun empty s
    where fun = \a -> \b -> appendA a b
    
test1 = Arr.fromList [1,2,3,4]
test2 = Arr.fromList [5,6,7]
test3 = Arr.fromList [8,9]
test4 = empty :: Arr Int
test5 = Arr.fromList [10,11,12]
test6 = empty :: Arr Int
test7 = singletonA 13
test = Arr.fromList [test1,test2,test3,test4,test5,test6,test7]

{-
-- Integer Logarithm base 2
ilg :: Int -> Int
ilg 1 = 0
ilg 2 = 1
ilg x = 1 + ilg (div x 2)

rdc :: (a -> a -> a) -> Arr a -> a
rdc f s
    | len == 1 = s!0
    | otherwise = let m = 2^(ilg (len - 1))
                      (l, r) = rdc f (takeA s m)
                               |||
                               rdc f (dropA s m)
                  in f l r
    where len = Arr.length s

reduceA :: (a -> a -> a) -> a -> Arr a -> a
reduceA f b s
    | Arr.length s == 0 = b
    | otherwise = f b (rdc f s)
-}
  
reduceA :: (a -> a -> a) -> a -> Arr a -> a
reduceA f b s
    | len == 0 = b
    | len == 1 = s!0
    | otherwise = reduceA f b (contract f s)
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

-- Contrae la secuencia dada con la operación dada, de a 2 elementos.
-- Si la secuencia tiene tamaño par se usa la función even_tab con tabulate:
-- recorre el arreglo de a 2 elementos y va calculando la función.
-- Si la secuencia tiene tamaño impar se usa la función odd_tab con tabulate:
-- recorre el arreglo de a 2 elementos, fijándose que el primero no sea el último
-- de la secuencia, en cuyo caso lo deja solo, si no, funciona como even_tab.
-- PREGUNTAR: tiene el mismo costo que tabulate?
-- len es O(1), ver si es par también (creo...) y después aplica tabulate
-- con funciones, de modo que el costo es el mismo que tabulate.
contract :: (a -> a -> a) -> Arr a -> Arr a
contract f s
    | isEven len = tabulate even_tab m
    | otherwise = tabulate odd_tab (1 + m)
    where len = Arr.length s
          m = div len 2
          even_tab i = f (s!(2*i)) (s!(2*i + 1)) -- O(W/S f)
          odd_tab i = if 2*i == (len - 1) then s!(2*i) -- O(W/S f)
                      else f (s!(2*i)) (s!(2*i + 1))

-- Nota: esto hace majia pura papá, este tp es usar tabulate siempre que puedas.
-- Hace lo que dice en los apuntes, básicamente.
-- PREGUNTAR: así es como debe hacerse?
combine :: (a -> a -> a) -> Arr a -> Arr a -> Arr a
--combine f xs s' = Arr.tabulate (\i -> if isEven i then s'!(div i 2) else f (s'!(div i 2)) (xs!(i-1))) len
combine f s s' = tabulate fun len
    where len = Arr.length s
          fun i = let m = div i 2
                  in if isEven i then s'!m
                     else f (s'!m) (s!(i - 1))

scanA :: (a -> a -> a) -> a -> Arr a -> (Arr a, a)
scanA f b s
    | len == 0 = (empty, b)
    | len == 1 = (singletonA b, f b (s!0))
    | otherwise = let (s', rdc) = scanA f b (contract f s)
                  in (combine f s s', rdc)
    where len = Arr.length s

-- Preguntar:
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
