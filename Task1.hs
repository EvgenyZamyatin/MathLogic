module Task1 where

import Verifier
import Parser
import Expression
import Util
import qualified Axioms as A

import System.IO  
import Control.Monad

main = do  
        let list = []
        handle <- openFile "task1.in" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
            list = singlewords
        print (zip list (verify A.axiomList [] (map parse list)))

        hClose handle   
