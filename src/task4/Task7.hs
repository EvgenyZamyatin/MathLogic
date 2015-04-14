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

format ('[':']':s) = (format s)
format ('[':s) = "(" ++ (format s)
format (']':s) = ")" ++ (format s)
format (c:s) = (c : (format s))
format "" = "";

toString1 :: String -> [Exp] -> String 
toString1 _ [] = ""
toString1 separator [e] = (show e)
toString1 separator (e:xs) = (show e) ++ separator ++ (toString1 separator xs)

toString2 :: [MaybeAnnotation] -> String 
toString2 [] = ""
toString2 [e] = (show e)
toString2 (e:xs) = (show e) ++ "\n" ++ (toString2 xs)



--checkLemma :: (Exp->[Exp])->Exp->[MaybeAnnotation]
checkLemma = (verify A.axiomList [])	
checkLemmaA a = (verify A.axiomList [a])


unpack (Not (EqualPredicate (Sum a b) c)) = (a, b, c)
unpack0 ((EqualPredicate (Sum a b) c)) = (a, b, c)

unpackMul (Mul a b) = (a, b)
down (Nxt a) = a
nxts 0 = Zero
nxts n = Nxt (nxts (n-1))

main = return (format (toString (zip (map show (letproov 15 2)) (checkLemma (letproov 15 2))))) >>= writeFile "task7.out"
main1 a b = return (format (toString (zip (map show (letproov a b)) (checkLemma (letproov a b))))) >>= writeFile "task7.out"


letproov a b = 
	if (mod a b == 0) 
		then 
			(proovd b (a `div` b) 0 a) ++ 
			[Impl (EqualPredicate (Mul (nxts b) ((nxts (a `div` b)))) (nxts a)) (Exist "z" (EqualPredicate (Mul (nxts b) (Var "z")) (nxts a)))] ++
			[(Exist "z" (EqualPredicate (Mul (nxts b) (Var "z")) (nxts a)))]
		else 
			proovnd a b



{-===================if a % b == 0==========================-}
proovd :: Int -> Int -> Int -> Int -> [Exp]
proovd n m k l 
	| (k > 0) = 
		let x = (proovd n m (k-1) (l-1)) in
		let y = last x in
		let (a, b, c) = unpack0 y in
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
		let x = (proovd n (m-1) n (l-1)) in
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


{-======================if a % b != 01===================================-}

--proovnd a b = 

proovnd a b = let tmp = (firstStep (Var "x") b a 0 a a) in
 	tmp ++
	(eat' b a a)


 	--(norm (last tmp))



proovForBack n m k l (EqualPredicate (Sum (Mul a b) c) d)
	| (k /= 0) = 
		let x = (EqualPredicate (Sum (Mul a b) c) d) in 
			subAtA5 (Mul a b) (down c) ++ 
			[x] ++
			(subAtA2 (Sum (Mul a b) c) (Nxt (Sum (Mul a b) (down c))) d) ++
			[Impl x (EqualPredicate (Nxt (Sum (Mul a b) (down c))) d)] ++ 
			[(EqualPredicate (Nxt (Sum (Mul a b) (down c))) d)] ++ 
			(subAtA3 (Sum (Mul a b) (down c)) (down d)) ++
			[EqualPredicate (Sum (Mul a b) (down c)) (down d)]
	| otherwise = 
		(removeZero (Sum (Mul a b) c) d) ++
		(subAtA8 a (down b)) ++
		(subAtA2 (Mul a b) (Sum (Mul a (down b)) (nxts n)) d) ++
		[Impl (EqualPredicate (Mul a b) d) (EqualPredicate (Sum (Mul a (down b)) (nxts n)) d)] ++
		[EqualPredicate (Sum (Mul a (down b)) (nxts n)) d] ++
		(proovForBack n (m-1) n l (EqualPredicate (Sum (Mul a (down b)) (nxts n)) d))

proovForBack n m k l (EqualPredicate ((Mul a b)) d) = 
		[(EqualPredicate ((Mul a b)) d)] ++
		(subAtA8 a (down b)) ++
		(subAtA2 (Mul a b) (Sum (Mul a (down b)) (nxts n)) d) ++
		[Impl (EqualPredicate (Mul a b) d) (EqualPredicate (Sum (Mul a (down b)) (nxts n)) d)] ++
		[EqualPredicate (Sum (Mul a (down b)) (nxts n)) d] ++
		(proovForBack n (m-1) n l (EqualPredicate (Sum (Mul a (down b)) (nxts n)) d))



firstStep zr n m k l cnt
	| l > 0 && k > 0 = 
		let tmp = firstStep zr n m (k-1) (l-1) cnt in
		let x = last tmp in 
		let (a,b,c) = unpack x in 
		let b' = Nxt b in 
		let c' = Nxt c in
		let t = tmp ++ 
			[(substitude [("A", last tmp), ("B", EqualPredicate (Sum a b') c')] (parse "#A->#B->#A"))] ++ 
			[(substitude [("A", last tmp), ("B", EqualPredicate (Sum a b') c')] (parse "#B->#A"))] in
		let y = (deductLast [EqualPredicate (Sum a b') c'] (proovForBack n m k l (EqualPredicate (Sum a b') c'))) in
			t ++
			y ++
			[substitude [("A", (EqualPredicate (Sum a b') c')), ("B", (EqualPredicate (Sum a b) c))] (parse "(#A->#B)->(#A->!#B)->!#A")] ++
			[substitude [("A", (EqualPredicate (Sum a b') c')), ("B", (EqualPredicate (Sum a b) c))] (parse "(#A->!#B)->!#A")] ++
			[substitude [("A", (EqualPredicate (Sum a b') c'))] (parse "!#A")]

	| (l > 0 && k == 0 && m == 0) = 
		let f = if (l /= cnt) then (Sum (Mul (nxts n) Zero) Zero) else ((Mul (nxts n) Zero)) in
		let t = 
			(removeZero (f) (nxts l)) ++
			[EqualPredicate f (nxts l)] ++
			(subAtA7 (nxts n)) ++
			(subAtA2 (((Mul (nxts n)) Zero)) (nxts l) (Zero)) ++
			[Impl (EqualPredicate (Mul (nxts n) Zero) Zero) (EqualPredicate (nxts l) (Zero))] ++
			[(EqualPredicate (nxts l) (Zero))] in
				(deductLast [EqualPredicate (f) (nxts l)] t) ++
				(deductLast [EqualPredicate (f) (nxts l)] (subAtA4 (nxts (l-1)))) ++
				[substitude [("A", (EqualPredicate (f) (nxts l))), ("B", (EqualPredicate (nxts l) (Zero)))] (parse "(#A->#B)->(#A->!#B)->!#A")] ++
				[substitude [("A", (EqualPredicate (f) (nxts l))), ("B", (EqualPredicate (nxts l) (Zero)))] (parse "(#A->!#B)->!#A")] ++
				[substitude [("A", (EqualPredicate (f) (nxts l))), ("B", (EqualPredicate (nxts l) (Zero)))] (parse "!#A")]

	| (l > 0) && (k == 0) = 
		let tmp = firstStep zr n (m-1) (n-1) (l-1) cnt in
		let x = last tmp in 
		let (a,b,c) = unpack x in 
		let b' = Nxt b in 
		let c' = Nxt c in
		let (o, p) = unpackMul a in 
		let f = if (l /= cnt) then EqualPredicate (Sum (Mul o (Nxt p)) Zero) c' else EqualPredicate ((Mul o (Nxt p))) c' in
		let g = if (l /= cnt) then EqualPredicate (Sum a b) c else EqualPredicate (Sum a b) c in
		let t = tmp ++ 
			[(substitude [("A", last tmp), ("B", f)] (parse "#A->#B->#A"))] ++ 
			[(substitude [("A", last tmp), ("B", f)] (parse "#B->#A"))] in
		let y = (deductLast [f] (proovForBack n m k l (f))) in
			t ++
			y ++
			[substitude [("A", (f)), ("B", g)] (parse "(#A->#B)->(#A->!#B)->!#A")] ++
			[substitude [("A", (f)), ("B", g)] (parse "(#A->!#B)->!#A")] ++
			[substitude [("A", (f))] (parse "!#A")]

	| (l == 0) = 
		let a = Mul (nxts n) (nxts' zr m) in
		let b = nxts k in 
		let t = 
			[EqualPredicate (Sum a b) Zero] ++
			(subAtA5 a (down b)) ++ 
			(subAtA2 (Sum a b) (Nxt (Sum a (down b))) Zero) ++ 
			[Impl (EqualPredicate (Sum a b) Zero) (EqualPredicate (Nxt (Sum a (down b))) Zero)] ++
			[(EqualPredicate (Nxt (Sum a (down b))) Zero)] in
				(deductLast [EqualPredicate (Sum a b) Zero] t) ++
				(deductLast [EqualPredicate (Sum a b) Zero] (subAtA4 ((Sum a (down b))))) ++
				[substitude [("A", EqualPredicate (Sum a b) Zero), ("B", (EqualPredicate (Nxt (Sum a (down b))) Zero))] (parse "(#A->#B)->(#A->!#B)->!#A")] ++
				[substitude [("A", (EqualPredicate (Sum a b) Zero)), ("B", (EqualPredicate (Nxt (Sum a (down b))) Zero))] (parse "(#A->!#B)->!#A")] ++
				[substitude [("A", (EqualPredicate (Sum a b) Zero))] (parse "!#A")]


nxts' t 0 = t
nxts' t n = Nxt (nxts' t (n-1))

eat' a 0 c = []
eat' a b c = 
	let zr = Not (EqualPredicate (Mul (nxts a) (nxts (b-1))) (nxts c)) in
	let x = Not (EqualPredicate (Mul (nxts a) (nxts' (Var "x") (b-1))) (nxts c)) in
	let y = Not (EqualPredicate (Mul (nxts a) (nxts' (Var "x") (b))) (nxts c)) in
		(firstStep Zero a (b-1) 0 c c) ++
		[(substitude [("A", x), ("B", y)] (parse "#B->#A->#B"))] ++
		[(substitude [("A", x), ("B", y)] (parse "#A->#B"))] ++
		(hangX (substitude [("A", x), ("B", y)] (parse "#A->#B"))) ++
		[(substitude [("A", zr), ("B", Each "x" (Impl x y))] (parse "#A->#B->#A&#B"))] ++
		[(substitude [("A", zr), ("B", Each "x" (Impl x y))] (parse "#B->#A&#B"))] ++
		[(substitude [("A", zr), ("B", Each "x" (Impl x y))] (parse "#A&#B"))] ++
		[(substitude [("A", zr), ("B", Each "x" (Impl x y)), ("C", x)] (parse "#A&#B->#C"))] ++
		[(substitude [("C", x)] (parse "#C"))] ++
		(eat' a (b-1) c)

	


hangX e = map ((substitude  [("T", e), ("E", parse "P->P->P")]) . parse) 
	[ "#T"
	, "#E"
	, "#T->#E->#T"
	, "#E->#T"
	, "#E->@x(#T)"
	, "@x(#T)"
	]

{-
norm (Not (EqualPredicate a c)) = 
	let t = 
		(atoA (EqualPredicate a c)) ++
		[Impl (EqualPredicate a c) (Each "x" (EqualPredicate a c))] 
		--[Impl (Exist "x" (EqualPredicate a c)) (Each "x" (EqualPredicate a c))] ++
		--[Exist "x" (EqualPredicate a c)] ++ 
		--[Each "x" (EqualPredicate a c)] ++ 
		--[Impl (Each "x" (EqualPredicate a c)) ((EqualPredicate a c))] ++
		--[((EqualPredicate a c))] in
		in
	--(removeZero (Sum a b) c) ++
	(deductLast [Exist "x" (EqualPredicate a c)] t) ++
	[(substitude [("A", Not (EqualPredicate a c)), ("B", Exist "x" (EqualPredicate a c))] (parse "#A->#B->#A"))] ++ 
	[(substitude [("A", Not (EqualPredicate a c)), ("B", Exist "x" (EqualPredicate a c))] (parse "#B->#A"))] ++
	[(substitude [("A", Exist "x" (EqualPredicate a c)), ("B", (EqualPredicate a c))] (parse "(#A->#B)->(#A->!#B)->!#A"))] ++
	[(substitude [("A", Exist "x" (EqualPredicate a c)), ("B", (EqualPredicate a c))] (parse "(#A->!#B)->!#A"))] ++
	[(substitude [("A", Exist "x" (EqualPredicate a c)), ("B", (EqualPredicate a c))] (parse "!#A"))] ++
	[(substitude [("A", Exist "x" (EqualPredicate a c)), ("B", (EqualPredicate a c))] (parse "(#A->!#B)->!#A"))]
-}

 
		











