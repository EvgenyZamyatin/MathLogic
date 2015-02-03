module Verifier where
import Expression
import Parser
import Util
import qualified Data.Map as M
import qualified Data.MultiMap as MM
import qualified Data.Set as S



data Annatation = NotProoved
								| ByAxiom Int
								| ByAssumption Int
								| ByModusPones Int Int
								deriving (Show)


genAnnotationFromAxioms :: [Exp] -> Exp -> Annatation
genAnnotationFromAxioms ax st = genAnnotationFromAxioms' 0 ax st
	where	
		genAnnotationFromAxioms' c [] st = NotProoved
		genAnnotationFromAxioms' c (a:ax) st= if match a st then ByAxiom c else genAnnotationFromAxioms' (c+1) ax st

genAnnotationFromAssuptions :: [Exp] -> Exp -> Annatation
genAnnotationFromAssuptions ax st = genAnnotationFromAssuptions' 0 ax st
	where	
		genAnnotationFromAssuptions' c [] st = NotProoved
		genAnnotationFromAssuptions' c (a:ax) st= if match a st then ByAssumption c else genAnnotationFromAssuptions' (c+1) ax st


genAnnotationByMP :: MM.MultiMap Exp (Pair Exp Int)  -> M.Map Exp Int -> Exp -> Annatation
genAnnotationByMP m s st = 
	let ls = MM.lookup st m 
		in foldl f NotProoved ls
			where
				f :: Annatation -> Pair Exp Int -> Annatation
				f x y = case fnd of 
					Nothing -> x
					Just t -> ByModusPones t (scn y)
					where fnd = M.lookup (frs y) s  

genAnnotation :: MM.MultiMap Exp (Pair Exp Int) -> M.Map Exp Int -> [Exp] -> [Exp] -> Exp -> Annatation
genAnnotation mm m ax as st = f (genAnnotationFromAxioms ax st) (genAnnotationFromAssuptions as st) (genAnnotationByMP mm m st) 
	where  
		f NotProoved NotProoved c = c
		f NotProoved b _ = b
		f a _ _ = a
		

verify :: [Exp] -> [Exp] -> [Exp] -> [Annatation]
verify ax as thrm = verify' 0 MM.empty M.empty ax as thrm
	where 
		verify' :: Int -> MM.MultiMap Exp (Pair Exp Int) -> M.Map Exp Int -> [Exp] -> [Exp] -> [Exp] -> [Annatation]
		verify' c mm m ax as []     = []
		verify' c mm m ax as (t:thrm) = 
			let an = genAnnotation mm m ax as t
				in case an of 
					NotProoved -> NotProoved : (verify' (c+1) mm m ax as thrm)
					_ -> case t of 
						(Impl a b) -> an : (verify' (c+1) (MM.insert b (Pair a c) mm) (M.insert t c m) ax as thrm)
						_ -> an : (verify' (c+1) mm m ax as thrm)
