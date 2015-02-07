module Main where

import Verifier
import Parser
import Expression
import Util
import qualified Axioms as A


import System.IO  
import Control.Monad
import System.Directory

toString :: [(String, Annotation)] -> String 
toString = toString' 0
	where
		toString' :: Int -> [(String, Annotation)] -> String 
		toString' x [] = ""
		toString' x ((a,b):xs) = "(" ++ (show x) ++ ")" ++ " " ++ a ++ " " ++ (show b) ++ "\n" ++ (toString' (x+1) xs)

main = 
	myCreateDirectory "../tests/HW1/out"
	>> 
	fmap lines (readFile ("../tests/HW1/test_list.txt")) >>= f  
	where
		f :: [String] -> IO ()
		f [s] = calc s >> putStrLn ("complete " ++ s)
		f (s:xs) = calc s >> putStrLn ("complete " ++ s) >> f xs

calc :: String -> IO ()
calc s = 
	fmap f (readFile ( "../tests/HW1/"++ s)) >>= writeFile ("../tests/HW1/out/" ++ s ++ ".out") 
		where
			f str = toString (zip list (verify A.axiomList [] (map parse list)))
				where list = lines str
