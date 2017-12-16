import Data.Char (isDigit)
import System.Environment


--main :: IO ()
--main = do a <- getArgs
  --        let p = a !! 0
    --      let c = a !! 1
      --    w <- getContents
        --  if length a == 2 && length p == 1 && isDigit (head p) && length c == 1 && head c `elem` "UDLRB"
          --   then putStr $ unlines $ move (lines w) (read p) (head c)
            -- else putStrLn "Parâmetros inválidos"

-- j - jogador, a - ação    
--move :: [String] -> Int -> Char -> [String]
--move m j a = moveAux m j a 0

moveAux :: [String] -> Int -> Char -> Int -> [a]
moveAux m j a c = if m !!c !!0 == j then  coordJ (drop 3 (m !!c)) else moveAux m j a (c+1)

coordJ :: String -> (Char,Char)
coordJ j = (head j, last j)



-- Usar !! para localizar elemento da string ex: "# #" !!1 = " " ou !!0 "#"