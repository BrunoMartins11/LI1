module Main where
    
import System.Environment
import Text.Read
import Data.Maybe
import Data.List
import System.Random

main :: IO ()
main = do a <- getArgs
          let s = readMaybe (a !! 0)
          let l = readMaybe (a !! 1)
          if length a == 2 && isJust s && isJust l && fromJust s >= 5 && odd (fromJust s)
           then putStr $ unlines $ mapa (fromJust s) (fromJust l)
           else putStrLn "Parâmetros inválidos"

{- | A função mapa cria o mapa  de jogo 
-}
mapa :: Int -> Int -> [String]
mapa d s = sep d (mapaAux d (0,0) t) ++ findB d (3,1) t ++ findF d (3,1) t
        where t = take (takeAux d) $ randomRs (0,99) (mkStdGen s)

{- | A função takeAux calcula o número de elementos aleatórios necessários para um mapa de dimensão d
-}
takeAux :: Int -> Int
takeAux d = if d<=5 then 0 else 4*(d-6) + ((d-4)^2) - ((div (d-5) 2)+1)^2 

{- | A função mapaAux produz uma String com todos os elementos do mapa ordenados da esquerda para a direita, de cima para baixo
-}
mapaAux :: Int -> (Int,Int) -> [Int] -> String
mapaAux 0 (x,y) t = " "
mapaAux d (x,y) t |y == d = [] -- Final mapa
                  |x == (d-1) = '#': mapaAux d (0,(y+1)) t -- Ultimo elemento de uma linha
                  |y == (d-1) = '#': mapaAux d ((x+1),y) t --  Ultima Linha
                  |y == 0 = '#': mapaAux d ((x+1),0) t -- 1ª linha
                  |x == 0 && (y == (d-2) || y == 1) = '#': mapaAux d (x+1,y) t -- 1º elemento da segunda e penultima linha
                  |(x == 1 || x == 2) && (y == (d-2) || y == 1) = ' ': mapaAux d (x+1,y) t -- 2º e 3º elemento da segunda e penultima linha
                  |(x == d-3 || x == d-2) && (y == (d-2) || y == 1) = ' ': mapaAux d (x+1,y) t -- Antepenultimo e penultimo elemento da segunda e penultima linha
                  |(x == 1 || x == d-2) && (y == 2 || y == d-3) = ' ': mapaAux d (x+1,y) t -- 2º e penultimo elemento da terceira e antepenultima linha
                  |x == 0 = '#': mapaAux d (1,y) t -- 1º elemento de uma linha
                  |(mod x 2 == 0 && mod y 2 == 0) = '#': mapaAux d ((x+1),y) t -- Elementos fixos das linhas e colunas impares
                  |otherwise = (powerup (head t)): mapaAux d ((x+1),y) (tail t) -- Elementos aleatórios


{- | A função powerup converte um elemento aleatório no respetivo caracter que deve aparecer no mapa
-}
powerup :: Int -> Char
powerup t |t == 0 || t == 1 = '?' -- Bomba
          |t == 2 || t == 3 = '?'-- Flame
          |t <= 39 = '?' -- Tijolo
          |otherwise = ' ' -- Vazio

{- | A função sep separa a String do mapa numa lista com d Strings de d elementos
-}
sep :: Int -> String -> [String]
sep d [] = []
sep  d l =  take d l : sep d (drop d l) 

{- | A função findB encontra as coordenadas das bombas
-}  
findB :: Int -> (Int,Int) -> [Int] -> [String]
findB d (x,y) [] = []
findB d (x,y) m |x == d-1                           = findB d (1,y+1) m -- troca de linha
                |((y == 1 || y == d-2) && x == d-3) = findB d (1,y+1) m -- Ultimos 3 elementos fixos da segunda e penultima linha
                |((y == 1 || y == d-2) && x == 1)   = findB d (3,y) m -- Primeiros 3 elementos fixos da segunda e penultima linha
                |((y == 2 || y == d-3) && x == d-2) = findB d (1,y+1) m -- Ultimos 2 elementos fixos da terceira e antepenultima linha
                |((y == 2 || y == d-3) && x == 1)   = findB d (3,y) m -- Primeiros 2 elementos fixos da terceira e antepenultima linha
                |(mod x 2 == 0 && mod y 2 == 0)     = findB d (x+1,y) m -- Pedras fixas nas linhas e colunas impares
                |head m == 0 || head m == 1         = ("+ " ++ (show x) ++ " " ++ (show y)): findB d (x+1,y) (tail m) -- encontra as bombas nos restantes elementos
                |otherwise = findB d (x+1,y) (tail m) -- elementos aleatórios que não são bombas

{- | A função findF encontra as coordenadas das flames
-}  
findF :: Int -> (Int,Int) -> [Int] -> [String]
findF d (x,y) [] = []
findF d (x,y) m |x == d-1 = findF d (1,y+1) m -- troca de linha
                |((y == 1 || y == d-2) && x == d-3) = findF d (1,y+1) m -- Ultimos 3 elementos fixos da segunda e penultima linha
                |((y == 1 || y == d-2) && x == 1)   = findF d (3,y) m -- Primeiros 3 elementos fixos da segunda e penultima linha
                |((y == 2 || y == d-3) && x == d-2) = findF d (1,y+1) m -- Ultimos 2 elementos fixos da terceira e antepenultima linha
                |((y == 2 || y == d-3) && x == 1)   = findF d (3,y) m -- Pedras fixas nas linhas e colunas impares
                |(mod x 2 == 0 && mod y 2 == 0)     = findF d (x+1,y) m -- Pedras fixas nas linhas e colunas impares
                |head m == 2 || head m == 3         =  ("! " ++ (show x) ++ " " ++ (show y)): findF d (x+1,y) (tail m) -- encontra as flames
                |otherwise                          = findF d (x+1,y) (tail m) -- elementos aleatórios que não são flames