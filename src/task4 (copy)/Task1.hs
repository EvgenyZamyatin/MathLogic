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

parseAssumtions :: String -> [Exp]
parseAssumtions = parseAssumtions' ""
	where
		parseAssumtions' buf ('|':'-':s) = [parse buf]
		parseAssumtions' buf (',':s) = (parse buf) : (parseAssumtions' "" s)
		parseAssumtions' buf (c:s) = parseAssumtions' (buf ++ [c]) s

main = readFile "task1.in" >>= (return . f) >>= writeFile "task1.out"
	where
		f s = let lst = lines s in
			let (h,t) = (parseAssumtions (head lst), map parse (tail lst)) in
				toString (zip (tail lst) (verify A.axiomList h t))

				
{-
main1 = 
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


check00 lem = 
	let (a, b) = (Var "A", Var "B") in
		let thrm = lem a b in
		return ((show a) ++ "," ++ (show b) ++ "\n" ++ (toString (zip (map show thrm) (verify A.axiomList [a, b] thrm)))) >>= writeFile "00.txt"

check10 lem = 
	let (a, b) = (Not (Var "A"), Var "B") in
		let thrm = lem a b in
		return ((show a) ++ "," ++ (show b) ++ "\n" ++ (toString (zip (map show thrm) (verify A.axiomList [a, b] thrm)))) >>= writeFile "10.txt"

check01 lem = 
	let (a, b) = ((Var "A"), Not (Var "B")) in
		let thrm = lem a b in
		return ((show a) ++ "," ++ (show b) ++ "\n" ++ (toString (zip (map show thrm) (verify A.axiomList [a, b] thrm)))) >>= writeFile "01.txt"

check11 lem = 
	let (a, b) = (Not (Var "A"), Not (Var "B")) in
		let thrm = lem a b in
		return ((show a) ++ "," ++ (show b) ++ "\n" ++ (toString (zip (map show thrm) (verify A.axiomList [a, b] thrm)))) >>= writeFile "11.txt"

check lem = check00 lem >> check10 lem >> check01 lem >> check11 lem 

tmp = let thrm = (deductLast (map parse ["!A", "!B", "B"]) ((intuit1 (Var "B") (Var "A")) ++ (map parse ["B", "!B", "!B->A", "A"])))
	in return (toString (zip (map show thrm) (verify [] [] thrm))) >>= writeFile "tmp.txt"

tmp1 = let thrm = ((intuit1 (Var "B") (Var "A")) ++ (map parse ["B", "!B", "!B->A", "A"]))
	in return (toString (zip (map show thrm) (verify [] [] thrm))) >>= writeFile "tmp.txt"
-}