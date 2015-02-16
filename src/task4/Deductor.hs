module Deductor where

import Parser
import Expression
import Util
import Verifier

import qualified Axioms as A
import qualified Data.Map as M



remake :: M.Map Int Exp -> Exp -> Exp -> Annotation -> [Exp]
remake m as e (ByAxiom x) = [e, substitude [("A", e), ("B", as)] (parse "#A->#B->#A"), Impl as e]
remake m as e (ByAssumption x) = 
	if as /= e then [e, substitude [("A", e), ("B", as)] (parse "#A->#B->#A"), Impl as e]
	else map ((substitude [("A", e)]) . parse) [
																							 "#A->(#A->#A)"
																							,"(#A->#A->#A)->(#A->(#A->#A)->#A)->(#A->#A)"
																							,"(#A->(#A->#A)->#A)->(#A->#A)"
																							,"#A->(#A->#A)->#A"
																							,"#A->#A"
																							]  

remake m as e (ByModusPones j k) = 
	let extract v = 
		case v of 
			Nothing -> (parse "This can't be")
			Just t -> t 
	in map (substitude [("A", as), ("B", extract (M.lookup j m)), ("C", e)] . parse) ["(#A->#B)->((#A->(#B->#C))->(#A->#C))"
																																									 ,"((#A->(#B->#C))->(#A->#C))"
																																									 ,"#A->#C"
																																									 ]

remake m as (Impl a (Each x b)) (ByEachMP j) = 
	let extract v = 
		case v of 
			Nothing -> (parse "This can't be")
			Just t -> t 
	in ((map (substitude [("A", as), ("B", a), ("C", b)] . parse) (lemma1 ++ ["#A&#B->@"++x++"#C"])) 
		++ ((map (substitude [("A", as), ("B", a), ("C", Each x b)] . parse) lemma2)))

remake m as (Impl (Exist x a) b) (ByExistMP j) = 
	let extract v = 
		case v of 
			Nothing -> (parse "This can't be")
			Just t -> t 
	in ((map (substitude [("A", as), ("B", a), ("C", b)] . parse) (lemma1 ++ lemma3)) 
		++ ((map (substitude [("A", a), ("B", as), ("C", b)] . parse) (lemma2 ++ ["?" ++ x ++ "#A->#B->#C"]) )))
		++ ((map (substitude [("A", Exist x a), ("B", as), ("C", b)] . parse) (lemma1 ++ lemma3))
		++ ((map (substitude [("A", as), ("B", Exist x a), ("C", b)] . parse) lemma2)))


deduct :: Exp -> [Exp] -> [Annotation] -> [Exp]
deduct = f' 0 M.empty
	where 
		f' _ _ _ [] [] = []
		f' c m as (e:el) (a:al) = (remake m as e a) ++ (f' (c+1) (M.insert c e m) as el al) 

extractAnnotation :: [MaybeAnnotation] -> [Annotation]
extractAnnotation ((Ok a):as) = (a : (extractAnnotation as))
extractAnnotation [] = []

deductLast :: [Exp] -> [Exp] -> [Exp]
deductLast a b = deduct (head a) b (extractAnnotation (verify A.axiomList a b))


lemma1 = ["#A&#B->#A&#B->#A&#B"
					,"(#A&#B->#A&#B->#A&#B)->(#A&#B->(#A&#B->#A&#B)->#A&#B)->#A&#B->#A&#B"
					,"(#A&#B->(#A&#B->#A&#B)->#A&#B)->#A&#B->#A&#B"
					,"#A&#B->(#A&#B->#A&#B)->#A&#B"
					,"#A&#B->#A&#B"
					,"#A&#B->#A"
					,"(#A&#B->#A)->#A&#B->#A&#B->#A"
					,"#A&#B->#A&#B->#A"
					,"#A&#B->#B"
					,"(#A&#B->#B)->#A&#B->#A&#B->#B"
					,"#A&#B->#A&#B->#B"
					,"(#A&#B->#A&#B)->(#A&#B->#A&#B->#A)->#A&#B->#A"
					,"(#A&#B->#A&#B->#A)->#A&#B->#A"
					,"#A&#B->#A"
					,"(#A&#B->#A&#B)->(#A&#B->#A&#B->#B)->#A&#B->#B"
					,"(#A&#B->#A&#B->#B)->#A&#B->#B"
					,"#A&#B->#B"
					,"#A->#B->#C"
					,"(#A->#B->#C)->#A&#B->#A->#B->#C"
					,"#A&#B->#A->#B->#C"
					,"(#A&#B->#A)->(#A&#B->#A->#B->#C)->#A&#B->#B->#C"
					,"(#A&#B->#A->#B->#C)->#A&#B->#B->#C"
					,"#A&#B->#B->#C"
					,"(#A&#B->#B)->(#A&#B->#B->#C)->#A&#B->#C"
					,"(#A&#B->#B->#C)->#A&#B->#C"
					,"#A&#B->#C"]

lemma2 = lines "#A->#B->#A&#B\n(#A->#B->#A&#B)->#A->#A->#B->#A&#B\n#A->#A->#B->#A&#B\n(#A->#B->#A&#B)->#B->#A->#B->#A&#B\n((#A->#B->#A&#B)->#B->#A->#B->#A&#B)->#A->(#A->#B->#A&#B)->#B->#A->#B->#A&#B\n#A->(#A->#B->#A&#B)->#B->#A->#B->#A&#B\n(#A->#A->#B->#A&#B)->(#A->(#A->#B->#A&#B)->#B->#A->#B->#A&#B)->#A->#B->#A->#B->#A&#B\n(#A->(#A->#B->#A&#B)->#B->#A->#B->#A&#B)->#A->#B->#A->#B->#A&#B\n#A->#B->#A->#B->#A&#B\n#A->#A->#A\n(#A->#A->#A)->(#A->(#A->#A)->#A)->#A->#A\n(#A->(#A->#A)->#A)->#A->#A\n#A->(#A->#A)->#A\n#A->#A\n#A->#B->#A\n(#A->#B->#A)->#A->#A->#B->#A\n#A->#A->#B->#A\n(#A->#A)->(#A->#A->#B->#A)->#A->#B->#A\n(#A->#A->#B->#A)->#A->#B->#A\n#A->#B->#A\n#B->#B->#B\n(#B->#B->#B)->#A->#B->#B->#B\n#A->#B->#B->#B\n(#B->#B->#B)->(#B->(#B->#B)->#B)->#B->#B\n((#B->#B->#B)->(#B->(#B->#B)->#B)->#B->#B)->#A->(#B->#B->#B)->(#B->(#B->#B)->#B)->#B->#B\n#A->(#B->#B->#B)->(#B->(#B->#B)->#B)->#B->#B\n(#A->#B->#B->#B)->(#A->(#B->#B->#B)->(#B->(#B->#B)->#B)->#B->#B)->#A->(#B->(#B->#B)->#B)->#B->#B\n(#A->(#B->#B->#B)->(#B->(#B->#B)->#B)->#B->#B)->#A->(#B->(#B->#B)->#B)->#B->#B\n#A->(#B->(#B->#B)->#B)->#B->#B\n#B->(#B->#B)->#B\n(#B->(#B->#B)->#B)->#A->#B->(#B->#B)->#B\n#A->#B->(#B->#B)->#B\n(#A->#B->(#B->#B)->#B)->(#A->(#B->(#B->#B)->#B)->#B->#B)->#A->#B->#B\n(#A->(#B->(#B->#B)->#B)->#B->#B)->#A->#B->#B\n#A->#B->#B\n(#B->#A)->(#B->#A->#B->#A&#B)->#B->#B->#A&#B\n((#B->#A)->(#B->#A->#B->#A&#B)->#B->#B->#A&#B)->#A->(#B->#A)->(#B->#A->#B->#A&#B)->#B->#B->#A&#B\n#A->(#B->#A)->(#B->#A->#B->#A&#B)->#B->#B->#A&#B\n(#A->#B->#A)->(#A->(#B->#A)->(#B->#A->#B->#A&#B)->#B->#B->#A&#B)->#A->(#B->#A->#B->#A&#B)->#B->#B->#A&#B\n(#A->(#B->#A)->(#B->#A->#B->#A&#B)->#B->#B->#A&#B)->#A->(#B->#A->#B->#A&#B)->#B->#B->#A&#B\n#A->(#B->#A->#B->#A&#B)->#B->#B->#A&#B\n(#A->#B->#A->#B->#A&#B)->(#A->(#B->#A->#B->#A&#B)->#B->#B->#A&#B)->#A->#B->#B->#A&#B\n(#A->(#B->#A->#B->#A&#B)->#B->#B->#A&#B)->#A->#B->#B->#A&#B\n#A->#B->#B->#A&#B\n(#B->#B)->(#B->#B->#A&#B)->#B->#A&#B\n((#B->#B)->(#B->#B->#A&#B)->#B->#A&#B)->#A->(#B->#B)->(#B->#B->#A&#B)->#B->#A&#B\n#A->(#B->#B)->(#B->#B->#A&#B)->#B->#A&#B\n(#A->#B->#B)->(#A->(#B->#B)->(#B->#B->#A&#B)->#B->#A&#B)->#A->(#B->#B->#A&#B)->#B->#A&#B\n(#A->(#B->#B)->(#B->#B->#A&#B)->#B->#A&#B)->#A->(#B->#B->#A&#B)->#B->#A&#B\n#A->(#B->#B->#A&#B)->#B->#A&#B\n(#A->#A)->(#A->#A->#B->#A&#B)->#A->#B->#A&#B\n(#A->#A->#B->#A&#B)->#A->#B->#A&#B\n#A->#B->#A&#B\n#A&#B->#C\n(#A&#B->#C)->#A->#A&#B->#C\n#A->#A&#B->#C\n(#A&#B->#C)->#B->#A&#B->#C\n((#A&#B->#C)->#B->#A&#B->#C)->#A->(#A&#B->#C)->#B->#A&#B->#C\n#A->(#A&#B->#C)->#B->#A&#B->#C\n(#A->#A&#B->#C)->(#A->(#A&#B->#C)->#B->#A&#B->#C)->#A->#B->#A&#B->#C\n(#A->(#A&#B->#C)->#B->#A&#B->#C)->#A->#B->#A&#B->#C\n#A->#B->#A&#B->#C\n(#B->#A&#B)->(#B->#A&#B->#C)->#B->#C\n((#B->#A&#B)->(#B->#A&#B->#C)->#B->#C)->#A->(#B->#A&#B)->(#B->#A&#B->#C)->#B->#C\n#A->(#B->#A&#B)->(#B->#A&#B->#C)->#B->#C\n(#A->#B->#A&#B)->(#A->(#B->#A&#B)->(#B->#A&#B->#C)->#B->#C)->#A->(#B->#A&#B->#C)->#B->#C\n(#A->(#B->#A&#B)->(#B->#A&#B->#C)->#B->#C)->#A->(#B->#A&#B->#C)->#B->#C\n#A->(#B->#A&#B->#C)->#B->#C\n(#A->#B->#A&#B->#C)->(#A->(#B->#A&#B->#C)->#B->#C)->#A->#B->#C\n(#A->(#B->#A&#B->#C)->#B->#C)->#A->#B->#C\n#A->#B->#C"
lemma3 = lines "#B&#A->#B&#A->#B&#A\n(#B&#A->#B&#A->#B&#A)->(#B&#A->(#B&#A->#B&#A)->#B&#A)->#B&#A->#B&#A\n(#B&#A->(#B&#A->#B&#A)->#B&#A)->#B&#A->#B&#A\n#B&#A->(#B&#A->#B&#A)->#B&#A\n#B&#A->#B&#A\n#B&#A->#A\n(#B&#A->#A)->#B&#A->#B&#A->#A\n#B&#A->#B&#A->#A\n#B&#A->#B\n(#B&#A->#B)->#B&#A->#B&#A->#B\n#B&#A->#B&#A->#B\n(#B&#A->#B&#A)->(#B&#A->#B&#A->#A)->#B&#A->#A\n(#B&#A->#B&#A->#A)->#B&#A->#A\n#B&#A->#A\n(#B&#A->#B&#A)->(#B&#A->#B&#A->#B)->#B&#A->#B\n(#B&#A->#B&#A->#B)->#B&#A->#B\n#B&#A->#B\n#A->#B->#A&#B\n(#A->#B->#A&#B)->#B&#A->#A->#B->#A&#B\n#B&#A->#A->#B->#A&#B\n(#B&#A->#A)->(#B&#A->#A->#B->#A&#B)->#B&#A->#B->#A&#B\n(#B&#A->#A->#B->#A&#B)->#B&#A->#B->#A&#B\n#B&#A->#B->#A&#B\n(#B&#A->#B)->(#B&#A->#B->#A&#B)->#B&#A->#A&#B\n(#B&#A->#B->#A&#B)->#B&#A->#A&#B\n#B&#A->#A&#B\n#A&#B->#C\n(#A&#B->#C)->#B&#A->#A&#B->#C\n#B&#A->#A&#B->#C\n(#B&#A->#A&#B)->(#B&#A->#A&#B->#C)->#B&#A->#C\n(#B&#A->#A&#B->#C)->#B&#A->#C\n#B&#A->#C"