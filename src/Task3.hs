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
{-
d = (return (toString (deMorgan (parse "!(a|b)")) "\n")) >>= (writeFile "d.txt")
c = (return (toString (contraposition (parse "a->b")) "\n")) >>= (writeFile "c.txt")
e = (return (toString (implToOr (parse "!A->B")) "\n")) >>= (writeFile "e.txt")
y = (return (toString (implToNotAnd (parse "A->!B")) "\n")) >>= (writeFile "y.txt")
z = (return (toString (andToNotImpl (parse "A&!B")) "\n")) >>= (writeFile "z.txt")
k = (return (toString (intuit2 (Var "A") (Var "B") ) "\n")) >>= (writeFile "k.txt")
r = (return (toString (andToNotOr (parse "!A&!B")) "\n")) >>= (writeFile "r.txt")
-}
find :: Exp -> Proof
find e = findProof (findAllVars e) [] e

main = readFile "task3.in" >>= (return . go . parse) >>= writeFile "task3.out" 

go e = ((show (find e)))