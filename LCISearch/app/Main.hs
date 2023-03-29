module Main where

import System.Environment (getArgs)   
import Lib (searchMain)
import Main.Utf8 (withUtf8)

main :: IO ()
main = withUtf8 $ do  
    args <- getArgs
    if length args < 2 then putStrLn inputError else searchMain (head args) (tail args)
    where
            inputError = "Falscher Input. Erwartet: Path_To_Json attr1 {attr2} {attr3} …"