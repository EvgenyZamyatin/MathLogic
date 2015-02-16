module Verifier where
import Expression
import Parser
import Util
import Axioms
import Matcher
import qualified Data.Map as M
import qualified Data.MultiMap as MM
import qualified Data.Set as S



data Annotation = NotProoved
								| ByAxiom Int
								| ByAssumption Int
								| ByModusPones Int Int
								| ByEachMP Int
								| ByExistMP Int
								deriving (Eq)

instance Show Annotation where
	show NotProoved = "Не доказано"
	show (ByAxiom x) = "Сх. акс. " ++ (show x)
	show (ByAssumption x) = "По предположению " ++ (show x)
	show (ByModusPones x y) = "M.P. " ++ (show x) ++ ", " ++ (show y)
	show (ByEachMP x) = "Each M.P. " ++ (show x)
	show (ByExistMP x) = "Exist M.P. " ++ (show x)




genAnnotationFromAxioms :: [Exp] -> Exp -> Annotation
genAnnotationFromAxioms ax st = 
	case genAnnotationFromAxioms' 0 ax st of 
		NotProoved -> if (isTenthAxiom st == NoError) then (ByAxiom 19) 
			else if (isEleventhAxiom st == NoError) then (ByAxiom 20) 
				else if (isTwentythAxiom st == NoError) then (ByAxiom 21) 
					else NotProoved
		ans -> ans
		where	
			genAnnotationFromAxioms' c [] st = NotProoved
			genAnnotationFromAxioms' c (a:ax) st= if (match a st == NoError) then ByAxiom c else genAnnotationFromAxioms' (c+1) ax st

genAnnotationFromAssuptions :: [Exp] -> Exp -> Annotation
genAnnotationFromAssuptions ax st = genAnnotationFromAssuptions' 0 ax st
	where	
		genAnnotationFromAssuptions' c [] st = NotProoved
		genAnnotationFromAssuptions' c (a:ax) st = if a == st then ByAssumption c else genAnnotationFromAssuptions' (c+1) ax st

genAnnotationByMP :: MM.MultiMap Exp (Pair Exp Int)  -> M.Map Exp Int -> Exp -> Annotation
genAnnotationByMP m s st = 
	let ls = MM.lookup st m 
		in foldl f NotProoved ls
			where
				f :: Annotation -> Pair Exp Int -> Annotation
				f x y = case fnd of 
					Nothing -> x
					Just t -> ByModusPones t (scn y)
					where fnd = M.lookup (frs y) s  

genAnnotationByEachMP :: M.Map Exp Int -> Exp -> Annotation
genAnnotationByEachMP m (Impl a (Each x b)) = 
	let ls = M.lookup (Impl a b) m
		in case ls of 
			Nothing -> NotProoved
			Just t  -> if not (isFree x a) then ByEachMP t else NotProoved
genAnnotationByEachMP m _ = NotProoved 
	
genAnnotationByExistMP :: M.Map Exp Int -> Exp -> Annotation
genAnnotationByExistMP m (Impl (Exist x a) b) = 
	let ls = M.lookup (Impl a b) m
		in case ls of 
			Nothing -> NotProoved
			Just t  -> if not (isFree x b) then ByExistMP t else NotProoved
genAnnotationByExistMP m _ = NotProoved

genAnnotation :: MM.MultiMap Exp (Pair Exp Int) -> M.Map Exp Int -> [Exp] -> [Exp] -> Exp -> Annotation
genAnnotation mm m ax as st = 
	f [(genAnnotationFromAxioms ax st), (genAnnotationFromAssuptions as st), (genAnnotationByMP mm m st), 
		(genAnnotationByEachMP m st), (genAnnotationByExistMP m st)]  
	where  
		f l = foldl (\x -> \y-> if (y /= NotProoved) then y else x) NotProoved l
		
verify :: [Exp] -> [Exp] -> [Exp] -> [Annotation]
verify ax as thrm = verify' 0 MM.empty M.empty ax as thrm
	where 
		verify' :: Int -> MM.MultiMap Exp (Pair Exp Int) -> M.Map Exp Int -> [Exp] -> [Exp] -> [Exp] -> [Annotation]
		verify' c mm m ax as []     = []
		verify' c mm m ax as (t:thrm) = 
			let an = genAnnotation mm m ax as t
				in case an of 
					NotProoved -> NotProoved : (verify' (c+1) mm m ax as thrm)
					_ -> case t of 
						(Impl a b) -> an : (verify' (c+1) (MM.insert b (Pair a c) mm) (M.insert t c m) ax as thrm)
						_ -> an : (verify' (c+1) mm (M.insert t c m) ax as thrm)
