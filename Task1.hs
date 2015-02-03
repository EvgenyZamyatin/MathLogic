module Task1 where

import Verifier
import Parser
import Expression
import Util
import qualified Axioms as A

import System.IO  
import Control.Monad

toString [] = ""
toString ((a,b):xs) = (show a) ++ " " ++ (show b) ++ "\n" ++ toString xs

main = do  
        let list = []
        handle <- openFile "task1.in" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
            list = singlewords
        writeFile "task1.out" (toString (zip list (verify A.axiomList [] (map parse list))))
        hClose handle