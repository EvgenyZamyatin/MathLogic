module Main where

import Verifier
import Parser
import Expression
import Util
import Deductor
import qualified Axioms as A
import Lemmas


import System.IO  
import Control.Monad
import System.Directory

toString :: [Exp] -> String -> String 
toString [] _ = ""
toString [e] separator = (show e)
toString (e:xs) separator = (show e) ++ separator ++ (toString xs separator)

d = (return (toString (deMorgan (parse "!(a|b)")) "\n")) >>= (writeFile "d.txt")
c = (return (toString (contraposition (parse "a->b")) "\n")) >>= (writeFile "c.txt")