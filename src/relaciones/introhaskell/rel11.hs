------------------------------------------------------------------------------- 
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA 
-- 
-- (completa y sustituye los siguientes datos) 
-- Titulación: Grado en Ingeniería .......................................... [Informática]. 
-- Alumno: APELLIDOS, NOMBRE 
-- Fecha de entrega:  DIA | MES | AÑO 
-- 
-- Relación de Ejercicios 1. Ejercicios resueltos: ..........  
--  
------------------------------------------------------------------------------- 

------------------------------------------------------------------------------- 
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA 
-- 
-- (completa y sustituye los siguientes datos) 
-- Titulación: Grado en Ingeniería .......................................... [Informática]. 
-- Alumno: APELLIDOS, NOMBRE 
-- Fecha de entrega:  DIA | MES | AÑO 
-- 
-- Relación de Ejercicios 1. Ejercicios resueltos: ..........  
--  
------------------------------------------------------------------------------- 
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Test.QuickCheck
import Data.Tuple (swap)
import ErrUtils (ghcExit)

esTerna :: Integer -> Integer -> Integer -> Bool
esTerna x y z 
    | (x^2 + y^2) == z^2 = True
    | otherwise = False

terna :: Integer -> Integer -> (Integer, Integer, Integer)
terna x y = ((x^2) - (y^2), 2*x*y, (x^2) + (y^2))

p_ternas x y = x>0 && y>0 && x>y ==> esTerna l1 l2 h 
  where
    (l1,l2,h) = terna x y

p_terna2 x y = x>0 && y>0 && x>y ==> esTerna2 (terna x y)
    where
        esTerna2 (x, y, z) = esTerna x y z


intercambia :: (a,b) -> (b, a)
intercambia (x, y) = (y, x)

ordena2 :: (Ord a) => (a, a) -> (a, a)
ordena2 (x, y)
    | x > y = (y, x)
    |otherwise = (x, y)

ordena3 :: (Ord a) => (a, a, a) -> (a, a, a)
ordena3 (x, y, z)
    | x > y = (y, fst aux, snd aux)
    |otherwise = (x, fst h, snd h)
        where 
            aux = ordena2 (x, z)
            h = ordena2 (y, z)

