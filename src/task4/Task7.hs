module Main where

import Verifier
import Parser
import Expression
import Util
import Deductor
import Matcher
import qualified Axioms as A
import Lemmas

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
		parseAssumtions' buf (';':s) = (parse buf) : (parseAssumtions' "" s)
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

main = readFile "task4.in" >>= (return . f) >>= writeFile "task4.out"
	where
		f s = let lst = lines s in
			let (h,t) = (parseAssumtions (head lst), map parse (tail lst)) in
				let proov = (verify A.axiomList h t) in
					let e = findError 0 proov in
						if (e == -1) then 
							toString (zip (tail lst) proov)
							else show (proov !! e)

main' = readFile "task4.in" >>= (return . f) >>= writeFile "task4.out"
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

--checkLemma :: (Exp->[Exp])->Exp->[MaybeAnnotation]
checkLemma = (verify A.axiomList [])


{-
left EqualPredicate a b = a
right EqualPredicate a b = b
-}
unpack (EqualPredicate (Sum a b) c) = (a, b, c)
unpackMul (Mul a b) = (a, b)
down (Nxt a) = a
nxts 0 = Zero
nxts n = Nxt (nxts (n-1))

main7 a b = return (format (toString1 "\n" (letProov a b))) >>= writeFile "task7.out"


letProov a b = 
	if (mod a b == 0) 
		then 
			(proov b (a `div` b) 0 a) ++ 
			[Impl (EqualPredicate (Mul (nxts b) ((nxts (a `div` b)))) (nxts a)) (Exist "z" (EqualPredicate (Mul (nxts b) (Var "z")) (nxts a)))] ++
			[(Exist "z" (EqualPredicate (Mul (nxts b) (Var "z")) (nxts a)))]
		else 
			[]



proov :: Int -> Int -> Int -> Int -> [Exp]
proov n m k l 
	| (k > 0) = 
		let x = (proov n m (k-1) (l-1)) in
		let y = last x in
		let (a, b, c) = unpack y in
		let b' = Nxt b in 
		let c' = Nxt c in
		let tmp = 
			x ++
			(subAtA1 (Sum a b) c) ++
			[(EqualPredicate (Nxt (Sum a b)) (Nxt c))] ++
			(subAtA5 a b) ++
			(reflection (Sum a b') (Nxt (Sum a b))) ++ 
			(subAtA2 (Nxt (Sum a b)) (Sum a b') c') ++
			[Impl (EqualPredicate (Nxt (Sum a b)) c') (EqualPredicate (Sum a b') c')] ++
			[(EqualPredicate (Nxt (Sum a b)) c')] ++ 
			[(EqualPredicate (Sum a b') c')] in
				tmp
	
	| m == 0 && k == 0 = 
		(subAtA7 (nxts n)) ++
		(addZero (Mul (nxts n) Zero) Zero)
	
	| otherwise = 
		let x = (proov n (m-1) n (l-1)) in
		let y = last x in
			x ++
			hadleC n m n l y

hadleC n m k l (EqualPredicate (Sum (Mul a b) c) d) =
	if (n /= k) then []
		else
			(subAtA8 a b) ++
			(reflection (Mul a (Nxt b)) (Sum (Mul a b) a)) ++
			(subAtA2 (Sum (Mul a b) a) (Mul a (Nxt b)) d) ++ 
			[Impl (EqualPredicate (Sum (Mul a b) a) d) (EqualPredicate (Mul a (Nxt b)) d)] ++
			[(EqualPredicate (Mul a (Nxt b)) d)] ++ 
			if (n*m /= l) then (addZero (Mul a (Nxt b)) d) else []






















