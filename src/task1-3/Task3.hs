module Main where

import Verifier
import Parser
import Expression
import Util
import Deductor
import qualified Axioms as A
import Lemmas
import Proover


import System.IO  
import Control.Monad
import System.Directory

toString :: String -> [Exp] -> String 
toString _ [] = ""
toString separator [e] = (show e)
toString separator (e:xs) = (show e) ++ separator ++ (toString separator xs)

find :: Exp -> Proof
find e = findProof (findAllVars e) [] e

main = readFile "task3.in" >>= (return . go . parse) >>= writeFile "task3.out" 

go e = let ans = find e 
	in case ans of
		Ok _ -> ((show ans))
		Fail _ -> "Высказывание ложно при " ++ (show ans)