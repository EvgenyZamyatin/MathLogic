module Verifier where
import Expression
import Parser
import Util
import Axioms
import Matcher
import qualified Data.Map as M
import qualified Data.MultiMap as MM
import qualified Data.Set as S



data Annotation = 
								ByAxiom Int
								| ByAssumption Int
								| ByModusPones Int Int
								| ByEachMP Int
								| ByExistMP Int
								deriving (Eq)

data MaybeAnnotation = Ok Annotation | Fail Error

instance Show MaybeAnnotation where
	show (Fail a) = (show a)
	show (Ok a) = show a

instance Show Annotation where
	show (ByAxiom x) = "Сх. акс. " ++ (show x)
	show (ByAssumption x) = "По предположению " ++ (show x)
	show (ByModusPones x y) = "M.P. " ++ (show x) ++ ", " ++ (show y)
	show (ByEachMP x) = "Each M.P. " ++ (show x)
	show (ByExistMP x) = "Exist M.P. " ++ (show x)

myMax :: MaybeAnnotation -> MaybeAnnotation -> MaybeAnnotation
myMax (Ok a) _ = Ok a
myMax _ (Ok b) = Ok b
myMax (Fail SomeError) b = b
myMax a (Fail SomeError) = a



genAnnotationFromAxioms :: [Exp] -> Exp -> MaybeAnnotation
genAnnotationFromAxioms ax st = 
	case genAnnotationFromAxioms' 0 ax st of 
		Fail _ -> foldl myMax (Fail SomeError) [if (isTenthAxiom st == NoError) then Ok (ByAxiom 19) else Fail (isTenthAxiom st), 
																					if (isEleventhAxiom st == NoError) then Ok (ByAxiom 20) else Fail (isEleventhAxiom st),
																					if (isTwentythAxiom st == NoError) then Ok (ByAxiom 21) else Fail (isTwentythAxiom st)]
		ans -> ans
		where	
			genAnnotationFromAxioms' c [] st = Fail SomeError
			genAnnotationFromAxioms' c (a:ax) st= 
				if (match a st == NoError) then 
					if (c > 9) then if (a == st) then Ok (ByAxiom c) else genAnnotationFromAxioms' (c+1) ax st
					else 
						Ok (ByAxiom c) 
				else genAnnotationFromAxioms' (c+1) ax st



genAnnotationFromAssuptions :: [Exp] -> Exp -> MaybeAnnotation
genAnnotationFromAssuptions ax st = genAnnotationFromAssuptions' 0 ax st
	where	
		genAnnotationFromAssuptions' c [] st = Fail SomeError
		genAnnotationFromAssuptions' c (a:ax) st = if a == st then Ok (ByAssumption c) else genAnnotationFromAssuptions' (c+1) ax st

genAnnotationByMP :: MM.MultiMap Exp (Pair Exp Int)  -> M.Map Exp Int -> Exp -> MaybeAnnotation
genAnnotationByMP m s st = 
	let ls = MM.lookup st m 
		in foldl f (Fail SomeError) ls
			where
				f :: MaybeAnnotation -> Pair Exp Int -> MaybeAnnotation
				f x y = case fnd of 
					Nothing -> x
					Just t -> Ok (ByModusPones t (scn y))
					where fnd = M.lookup (frs y) s  

genAnnotationByEachMP :: M.Map Exp Int -> Exp -> MaybeAnnotation
genAnnotationByEachMP m (Impl a (Each x b)) = 
	let ls = M.lookup (Impl a b) m
		in case ls of 
			Nothing -> Fail SomeError
			Just t  -> if not (isFree x a) then Ok (ByEachMP t) else Fail (Error2 x a)
genAnnotationByEachMP m _ = Fail SomeError
	
genAnnotationByExistMP :: M.Map Exp Int -> Exp -> MaybeAnnotation
genAnnotationByExistMP m (Impl (Exist x a) b) = 
	let ls = M.lookup (Impl a b) m
		in case ls of 
			Nothing -> Fail SomeError
			Just t  -> if not (isFree x b) then Ok (ByExistMP t) else Fail (Error2 x b)
genAnnotationByExistMP m _ = Fail SomeError

genAnnotation :: MM.MultiMap Exp (Pair Exp Int) -> M.Map Exp Int -> [Exp] -> [Exp] -> Exp -> MaybeAnnotation
genAnnotation mm m ax as st = 
	f [(genAnnotationFromAxioms ax st), (genAnnotationFromAssuptions as st), (genAnnotationByMP mm m st), 
		(genAnnotationByEachMP m st), (genAnnotationByExistMP m st)]  
	where  
		f l = foldl myMax (Fail SomeError) l
		
verify :: [Exp] -> [Exp] -> [Exp] -> [MaybeAnnotation]
verify ax as thrm = verify' 0 MM.empty M.empty ax as thrm
	where 
		verify' :: Int -> MM.MultiMap Exp (Pair Exp Int) -> M.Map Exp Int -> [Exp] -> [Exp] -> [Exp] -> [MaybeAnnotation]
		verify' c mm m ax as []     = []
		verify' c mm m ax as (t:thrm) = 
			let an = genAnnotation mm m ax as t
				in case an of 
					Fail _ -> an : (verify' (c+1) mm m ax as thrm)
					_ -> case t of 
						(Impl a b) -> an : (verify' (c+1) (MM.insert b (Pair a c) mm) (M.insert t c m) ax as thrm)
						_ -> an : (verify' (c+1) mm (M.insert t c m) ax as thrm)
