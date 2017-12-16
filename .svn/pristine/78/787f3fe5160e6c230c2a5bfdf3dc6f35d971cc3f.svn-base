module Main where
import System.Environment
import Data.Char

encode :: [String] -> String
encode l = unlines (encodeAux l)

{- | A função encodeAux transforma a primeira String (de '#') na dimensão do mapa , aplica as funções remPrimUlt, remMeio e remEspa às restantes strings do mapa,
e aplica a função remEspaPU às strings restantes
-}
encodeAux :: [String] -> [String] -- ['######',....]
encodeAux [] = []
encodeAux (h:t) = (show (length h)): remEspa(remMeio(remPrimUlt (take (length h-2) t)) 0) ++ remEspaPU(drop (length h-1) t) -- [6,....]

{- | A função remPrimUlt remove o primeiro e ultimo elemento de cada String, ou seja, os '#' iniciais e finais de cada linha
-}
remPrimUlt :: [String] -> [String]
remPrimUlt [] = []
remPrimUlt (h:ts) = take (length h-2) (drop 1 h): remPrimUlt ts

{- | A função remMeio seleciona as strings corretas para que os '#' do meio fixos sejam removidos
-}
remMeio :: [String] -> Int -> [String]
remMeio [] _ = []
remMeio (h:ts) c |c == 0 = h: remMeio ts 1
                 |c == 1 = (remMeioAux h 0): remMeio ts 0

{- | A função remMeioAux remove os '#' fixos da string (elementos impares)
-}
remMeioAux :: String -> Int -> String
remMeioAux "" _ = ""
remMeioAux (h:ts) c |c == 0 = h: remMeioAux ts 1
                    |c == 1 = remMeioAux ts 0

{- | A função remEspa aplica a função remEspaAux a cada string da lista
-}
remEspa :: [String] -> [String]
remEspa [] = []
remEspa (h:ts) = (remEspaAux h 0): remEspa ts

{- | A função remEspaAux subsitui os espaços da string pelo valor de espaços consecutivos
-}
remEspaAux :: String -> Int -> String
remEspaAux [] c = if c == 0 then [] else show c
remEspaAux (h:ts) c |h == ' ' = remEspaAux ts (c+1)
                    |c == 0 = h: remEspaAux ts 0
                    |otherwise = (show c) ++ h: remEspaAux ts 0

{- | A função remEspaPU aplica a função remEspaPUAux a todas as String da lista (strings dos powerups)
-}
remEspaPU :: [String] -> [String]
remEspaPU [] = []
remEspaPU (h:ts) = if (head h) == '+' || (head h) == '!' then remEspaPUAux h "": remEspaPU ts else h: remEspaPU ts

{- | A função remEspaPUAux remove os espaços de cada String de powerups e dispõe os seu elementos de modo a que "! x y" fique "x!y"
-}
remEspaPUAux :: String -> String -> String
remEspaPUAux [x] _ = [x]
remEspaPUAux (h:ts) c |h == '+' || h == '!' = remEspaPUAux (tail ts) (c ++ [h])
                      |isDigit h = h: remEspaPUAux ts c
                      |otherwise = c ++ remEspaPUAux ts c

decode :: String -> [String]
decode l = decodeAux (lines l)

{- | A função decodeAux utiliza a dimensão do mapa para criar as strings de '#' fixos, aplica as funções addEspa, addMeio e add PrimUlt às restantes strings do mapa,
e aplica a função addEspaPU às strings restantes
-}
decodeAux :: [String] -> [String]
decodeAux [] = []
decodeAux (h:t) = (replicate (read h) '#'): addPrimUlt (addMeio (addEspa(take (read h-2) t)) 0) ++ (replicate (read h) '#'): addEspaPU(drop (read h-2) t)

{- | A função addEspa aplica a função addEspaAux a cada string da lista
-}
addEspa :: [String] -> [String]
addEspa [] = []
addEspa (h:ts) = addEspaAux h "0": addEspa ts

checkString (h:ts) = isDigit
{- | A função addESpaAux substitui os valores da string por esse número de espaços
-}
addEspaAux :: String -> String -> String
addEspaAux [] c = if c == "0" then [] else replicate (read c) ' '
addEspaAux (h:ts) c | isDigit h = addEspaAux ts (c++[h])
                    | c == "0" = h: addEspaAux ts "0"
                    | otherwise = replicate (read c) ' '++ [h] ++ addEspaAux ts "0"

{- | A função addMeio seleciona as strings corretas para adicionar os '#' fixos do meio
-}
addMeio :: [String] -> Int -> [String]
addMeio [] _ = []
addMeio (h:ts) c |c == 0 = h:addMeio ts 1
                 |c == 1 = (addMeioAux h 0): addMeio ts 0

{- | A função addMeioAux adiciona os '#' fixos da string (elementos impares)
-}
addMeioAux :: String -> Int -> String
addMeioAux "" _ = ""
addMeioAux (h:ts) c |c == 0 = h: addMeioAux ts 1
                    |c == 1 = ['#'] ++ h: addMeioAux ts 1

{- | A função addPrimUlt adiciona a cada String o '#' inicial e final
-}
addPrimUlt :: [String] -> [String]
addPrimUlt [] = []
addPrimUlt (h:ts) = [['#']++ h ++ ['#']] ++ addPrimUlt ts

{- | A função addEspaPU aplica a função addEspaPUAux às Strings da lista que sejam coordenadas de powerups
-}
addEspaPU :: [String] -> [String]
addEspaPU [] = []
addEspaPU (h:ts) |not (head (tail h) == ' ') = addEspaPUAux h "0": addEspaPU ts
                 |otherwise = h: addEspaPU ts

{- | A função addEspaPUAux adiciona os espaços de cada String de powerups e dispõe os seu elementos de modo a que "x!y" fique "! x y" 
-}
addEspaPUAux :: String -> String -> String 
addEspaPUAux [x] c = ' ': show(read (c ++ [x]) :: Int)
addEspaPUAux (h:ts) c |h == '+' || h == '!' = h: ' ': show(read c :: Int) ++ addEspaPUAux ts "0"
                      |otherwise = addEspaPUAux ts (c ++ [h])

main :: IO ()
main = do a <- getArgs
          let p = a !! 0
          w <- getContents
          if length a == 1 && length p == 2 && (p=="-e" || p=="-d")
             then if p=="-e" then putStr $ encode $ lines w
                             else putStr $ unlines $ decode w
             else putStrLn "Parâmetros inválidos"
