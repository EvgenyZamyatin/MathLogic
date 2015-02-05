module Main where

import Verifier
import Parser
import Expression
import Util
import Deductor
import qualified Axioms as A


import System.IO  
import Control.Monad
import System.Directory

toString :: [Exp] -> String -> String 
toString [] _ = ""
toString [e] separator = (show e)
toString (e:xs) separator = (show e) ++ separator ++ (toString xs separator)

toStringWithAnnotations :: [(Exp, Annotation)] -> String 
toStringWithAnnotations [] = ""
toStringWithAnnotations ((a,b):xs) = (show a) ++ " " ++ (show b) ++ "\n" ++ (toStringWithAnnotations xs)

parseAssumtions :: String -> [Exp]
parseAssumtions = parseAssumtions' ""
	where
		parseAssumtions' buf ('|':'-':s) = [parse buf]
		parseAssumtions' buf (',':s) = (parse buf) : (parseAssumtions' "" s)
		parseAssumtions' buf (c:s) = parseAssumtions' (buf ++ [c]) s

main = myCreateDirectory "../tests/HW2/out"
	>> 
	fmap lines (readFile ("../tests/HW2/test_list.txt")) >>= f  
	where
		f :: [String] -> IO ()
		f [s] = calcWithAnnotations s >> putStrLn ("complete " ++ s)
		f (s:xs) = calcWithAnnotations s >> putStrLn ("complete " ++ s) >> f xs


calc :: String -> IO ()
calc s = 
	fmap f (readFile ( "../tests/HW2/"++ s)) >>= writeFile ("../tests/HW2/out/" ++ s ++ ".out") 
		where 
			f str = 
				let list = lines str in
					let assumt = (parseAssumtions . head) list in 
						let exps = map parse (tail list) in
							(toString (init assumt) ",") ++ "|-" ++ ( show (Impl (last assumt) (last exps)) ) ++ "\n" ++ (toString (deduct (last assumt) exps (verify A.axiomList assumt exps)) "\n")

calcWithAnnotations :: String -> IO ()
calcWithAnnotations s = 
	fmap f (readFile ( "../tests/HW2/"++ s)) >>= writeFile ("../tests/HW2/out/" ++ s ++ ".out") 
		where 
			f str = 
				let list = lines str in
					let assumt = (parseAssumtions . head) list in 
						let exps = map parse (tail list) in
							let nexps = deduct (last assumt) exps (verify A.axiomList assumt exps) in
							(toString (init assumt) ",") ++ "|-" ++ ( show (Impl (last assumt) (last exps)) ) ++ "\n"
							 ++ (toStringWithAnnotations (zip nexps (verify A.axiomList (init assumt) nexps)))
							 												