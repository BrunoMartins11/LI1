module Main where

import Data.Char (isDigit)
import System.Environment
import Text.Read
import Data.Maybe
import Data.List

{- | A função avanca devolve o estado de jogo após as alterações nesse instante
-}
avanca :: [String] -> Int -> [String]
avanca m t = if t <= ((veDim m)-2)^2 then tiraSegundo (verificaTempo  (espiral m (1,1) (1,0) ((veDim m) - 1) (((t-(veDim m)-2)^2)+1) ) []) else verificaTempo (tiraSegundo  m)  []

{- | A função tiraSegundo retira 1 segundo ao tempo restante de cada bomba
-}
tiraSegundo :: [String] -> [String]
tiraSegundo [] = []
tiraSegundo ((h:t):ts) = if (h=='*') then (unwords (take 5 (words (h:t))) ++ " " ++ unwords [show (read (last (words t)) - 1)]): tiraSegundo ts else (h:t): tiraSegundo ts


{- | A função verificaTempo verifica as bombas que têm 0 segundos restantes para explodir (vão explodir nesse instante)
-}
verificaTempo :: [String] -> [String] -> [String]
verificaTempo [] acc = acc
verificaTempo ((h:t):ts) acc = if h == '*' && last (words (h:t)) == show 0 then verificaTempo (resultadoBomba (acc++(h:t):ts) (read ((words(h:t))!!1),read ((words(h:t))!!2)) (read ((words(h:t))!!4)) 0 ) [] else verificaTempo ts (acc++[(h:t)])

{- | A função resultadoBomba recebe o mapa, a coordenada da explosão, o seu raio de explosão, e um acumulador e aplica as funções retiraBomba e retiraJogadores na coordenada da explosão e a função resultadoExplosão para cada direção
-}
resultadoBomba :: [String] -> (Int,Int) -> Int -> Int -> [String]
resultadoBomba m (x,y) r 0   = resultadoBomba (retiraJogadores (retiraBomba m (x,y)) (x,y)) (x,y) r 1
resultadoBomba m (x,y) r acc = resultadoExplosao (resultadoExplosao (resultadoExplosao (resultadoExplosao m (x,y) r (1,0) acc) (x,y) r (-1,0) acc) (x,y) r (0,1) acc) (x,y) r (0,-1) acc

{- | A função resultadoExplosao recebe o mapa, a coordenada que a função vai afetar, o raio da explosão, o vetor da direção, e um acumulador e através de condições altera o estado de jogo
-}
resultadoExplosao :: [String] -> (Int,Int) -> Int -> (Int,Int) -> Int -> [String]
resultadoExplosao m (x,y) r (j,i) acc 
    | daPosicao m (x,y) == '#' = m 
    | daPosicao m (x,y) == '?' = retirarTijolo m (x,y) 0
    | sePwrUp m (x,y) && daPosicao m (x,y)== ' ' = resultadoExplosao (tiraPwrUp m2 (x,y)) (x+j,y+i) r (j,i) (acc+1)
    | seBomba m (x,y) = resultadoExplosao (reduzContador m2 (x,y)) (x+j,y+i) r (j,i) (acc+1)
    | otherwise = if acc <= r then resultadoExplosao m2 (x+j,y+i) r (j,i) (acc+1)
                             else m2
             where m2 = retiraJogadores m (x,y)

{- | A função reduzContador reduz o tempo restante das bombas atingidas por uma explosão para 1
-}
reduzContador :: [String] -> (Int,Int) -> [String]
reduzContador [] _ = []
reduzContador ((h:t):ts) (x,y) = if h == '*' && words (h:t)!!1 == show x && words (h:t)!!2 == show y then [unwords $ take 5 (words(h:t)) ++ "1": ts] else (h:t): reduzContador ts (x,y)

{- | A função daPosicao devolve o caracter que se encontra no mapa numa dada coordenada
-}
daPosicao :: [String] -> (Int,Int) -> Char
daPosicao m (x,y) = (m!!y)!!x

{- | A função retirarTijolo remove o tijolo ('?') atingido pela explosão
-}
retirarTijolo :: [String] -> (Int,Int) -> Int -> [String] 
retirarTijolo [] _ n = []
retirarTijolo (h:t) (x,y) n = if n == y then (retirarTijolo2 h x 0) : t 
                                        else h : retirarTijolo t (x,y) (n+1) 
          where retirarTijolo2 :: String -> Int -> Int -> String
                retirarTijolo2 (h:t) x p = if x == p then ' ' : t
                                                     else h : retirarTijolo2 t x (p+1)                  

{- | A função sePwrUp verifica se existe no estado de jogo um power up com as coordenadas dadas e devolve um Bool
-}
sePwrUp :: [String] -> (Int,Int) -> Bool
sePwrUp [] (x,y) = False
sePwrUp ((h:t):ts) (x,y) = if (((h == '+') || (h == '!')) && ((read((words (h:t))!!1)) == x) && ((read((words (h:t))!!2)) == y)) then True 
                                                                                                                              else sePwrUp ts (x,y)

{- | A função seBomba verifica se existe no estado de jogo uma bomba com as coordenadas dadas e devolve um Bool
-} 
seBomba :: [String] -> (Int,Int) -> Bool
seBomba [] (x,y) = False
seBomba ((h:t):ts) (x,y) = if (h == '*') && ((read((words (h:t))!!1)) == x) && ((read((words (h:t))!!2)) == y) then True 
                                                                                                                else seBomba ts (x,y)
{- | A função retiraBomba remove do estado de jogo a bomba que explode 
-}
retiraBomba :: [String] -> (Int,Int) -> [String]
retiraBomba [] _ = []
retiraBomba (h:t) (x,y) = if (h!!0)== '*' && (read (words h !! 1)) == x && (read (words h !! 2)) == y  then retiraBomba t (x,y) 
                                                                                                       else h : retiraBomba t (x,y)

{- | A função retiraJogadores remove do estado de jogo um jogador atingido pela explosão da bomba
-}
retiraJogadores :: [String] -> (Int,Int) -> [String]
retiraJogadores [] _ = []
retiraJogadores (h:t) (x,y) = if isDigit (h!!0) && (read (words h !! 1)) == x && (read (words h !! 2)) then retiraJogadores t (x,y)
                                                                                                       else h : retiraJogadores t (x,y)

{- | A função tiraPwrUp remove do estado de jogo um powerup atingido pela explosão da bomba
-}
tiraPwrUp :: [String] -> (Int,Int) -> [String]
tiraPwrUp [] _ = []
tiraPwrUp ((h:t):ts) (x,y) = if ((h=='+') || (h=='!')) && (read ((words (h:t))!!1) == x) && (read ((words (h:t))!!2) == y) then tiraPwrUp ts (x,y) else (h:t) : tiraPwrUp ts (x,y)

--ESPIRAL-----

{- | A função espiral recebe o mapa, a posição inicial (1,1) da espiral, um vetor de movimento, um acumulador e o numero de blocos que vão ser substituidos
-}
espiral :: [String] -> (Int,Int) -> (Int,Int) -> Int -> Int ->  [String]
espiral m _ _ _ 0 = mataTudo m []
espiral m (x,y) (j,i) acc blocos = espiral (mudaPos m (x,y) '#' 0) (x+j,y+i) novoVetor novoacc (blocos-1)
                              where (novoVetor,novoacc) =  verificaBorda (veDim m) (x,y) (j,i) acc

{- | A função veDim dá a dimensão do mapa
-}
veDim :: [String] -> Int
veDim m = length (m!!0)

{- | A função verificaBorda verifica se os blocos devem ser começado a ser substituidos noutra direção
-}
verificaBorda :: Int -> (Int,Int) -> (Int,Int) -> Int -> ((Int,Int),Int)
verificaBorda dim (x,y) (1,0) acc    = if (x+1) == (acc-1) then ((0,1),acc)        else ((1,0),acc)
verificaBorda dim (x,y) (0,1) acc    = if (y+1) == (acc-1) then (((-1),0),acc)     else ((0,1),acc)                                 
verificaBorda dim (x,y) ((-1),0) acc = if (x-1) == (dim - acc) then ((0,(-1)),(acc-1)) else (((-1),0),acc)
verificaBorda dim (x,y) (0,(-1)) acc = if (y-1) == (dim - acc) then ((1,0),acc)        else ((0,(-1)),acc) 

{- | A função mataTudo remove os powerups e os jogadores do estado de jogo
-}
mataTudo :: [String] -> [String] -> [String]
mataTudo [] acc = acc
mataTudo (h:t) acc = if elem (h!!0) ['+','!','0','1','2','3','4'] then if (acc!!x!!y == '#') then mataTudo t acc else mataTudo t (acc ++ [h]) else mataTudo t (acc ++ [h])
                                      where (x,y) = ((read ((words h)!!1)),(read ((words h)!!2)))

{- | A função mudaPos recebe o mapa, a coordenada que vai ser substituida, o caracter que ser utilizado e encontra a linha correta do mapa e aplica a função mudaPosx a essa linha
-}
mudaPos :: [String] -> (Int,Int) -> Char -> Int -> [String] --um mapa, a posiçao de mudaça, o caracter que vai entrar e devolve o mapa alterado
mudaPos [] _ _ _ = []
mudaPos (h:t) (x,y) c acc = if acc == y then (mudaPosx h x c 0):t else h: mudaPos t (x,y) c (acc+1)

{- | A função mudaPos recebe a linha do mapa, a coordenada que vai ser substituida, o caracter que ser utilizado e substituir esse caracter
-}
mudaPosx :: String -> Int -> Char -> Int -> String
mudaPosx [] _ _ _ = []
mudaPosx (hx:tx) x c accx = if accx == x then (c : tx) else hx: mudaPosx tx x c (accx + 1)   


{-| Função que imprime o mapa
-}
main :: IO ()
main = do
    a <- getArgs
    let ticks = readMaybe (a !! 0)
    w <- getContents
    if isJust ticks
        then putStr $ unlines $ avanca (lines w) (fromJust ticks)
        else putStrLn "Parâmetros inválidos"