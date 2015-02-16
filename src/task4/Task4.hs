module Main where

import Verifier
import Parser
import Expression
import Util
import Deductor
import Matcher
import qualified Axioms as A


import System.IO  
import Control.Monad
import System.Directory

toString :: [(String, MaybeAnnotation)] -> String 
toString = toString' 0
	where
		toString' :: Int -> [(String, MaybeAnnotation)] -> String 
		toString' x [] = ""
		toString' x ((a,b):xs) = "(" ++ (show x) ++ ")" ++ " " ++ a ++ " " ++ (show b) ++ "\n" ++ (toString' (x+1) xs)

parseAssumtions :: String -> [Exp]
parseAssumtions = parseAssumtions' ""
	where
		parseAssumtions' buf ('|':'-':s) = if buf == [] then [] else [parse buf]
		parseAssumtions' buf (',':s) = (parse buf) : (parseAssumtions' "" s)
		parseAssumtions' buf (c:s) = parseAssumtions' (buf ++ [c]) s

format ('[':']':s) = (format s)
format ('[':s) = "(" ++ (format s)
format (']':s) = ")" ++ (format s)
format (c:s) = (c : (format s))
format "" = "";



findError c [] = -1
findError c (a:ma) = 
	case a of 
		Fail _ -> c
		_ -> findError (c+1) ma

main' = readFile "task1.in" >>= (return . f) >>= writeFile "task1.out"
	where
		f s = let lst = lines s in
			let (h,t) = (parseAssumtions (head lst), map parse (tail lst)) in
				let proov = (verify A.axiomList h t) in
					let e = findError 0 proov in
						if (e == -1) then toString (zip (tail lst) proov)
							else show (proov !! e)

main = readFile "task1.in" >>= (return . f) >>= writeFile "task1.out"
	where
		f s = let lst = lines s in
			let (h,t) = (parseAssumtions (head lst), map parse (tail lst)) in
				let proov = (verify A.axiomList h t) in
					let e = findError 0 proov in
						if (e == -1) then let r = check 0 t (h) in if (r /= NoError) then e3 r else format ((toString1 "," (tail h)) ++ "|-" ++ (show (last ((if (h /= []) then (deductLast (reverse h) t) else t)))) ++ "\n"++(toString1 "\n" (if (h /= []) then (deductLast (reverse h) t) else t)))
							else "Исходное доказательство некорректно начиная с формулы " ++ (show e) ++ "[: " ++ (show (proov !! e))++"]"

e3 (Error3 a b c) = "В выводе по правилу дедукции используется правило вывода с квантером по переменной " ++ a ++ ",которая входит свободно в допущение " ++ (show b)

check c s t = if (t == []) then NoError else (check' c s (last t))
check' c (x:xs) as =
	case x of
		Impl a (Each s b) -> if (isFree s as) then Error3 s as c else check' (c+1) xs as
		Impl (Exist s a) b -> if (isFree s as) then Error3 s as c else check' (c+1) xs as
		--Impl (Each s a) b -> if (isFree s as) then Error3 s as c else check (c+1) xs as
		--Impl a (Exist s b)  -> if (isFree s as) then Error3 s as c else check (c+1) xs as
		_ -> check' (c+1) xs as
check' c [] as = NoError


toString1 :: String -> [Exp] -> String 
toString1 _ [] = ""
toString1 separator [e] = (show e)
toString1 separator (e:xs) = (show e) ++ separator ++ (toString1 separator xs)


				
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