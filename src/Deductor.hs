module Deductor where

import Parser
import Expression
import Util
import Verifier
import qualified Axioms as A
import qualified Data.Map as M


substitude :: [(String, Exp)] -> Exp -> Exp
substitude list tmpl = f (M.fromList list) tmpl 
	where 
		f m (Var v) = 
			let e = M.lookup v m in 
				case e of 
					Nothing -> Var v
					(Just t) -> t
		f m (Not x) = Not (f m x)
		f m (And x y) = And (f m x) (f m y)
		f m (Or x y) = Or (f m y) (f m y)
		f m (Impl x y) = Impl (f m x) (f m y)


remake :: M.Map Int Exp -> Exp -> Exp -> Annotation -> [Exp]
remake m as e (ByAxiom x) = [e, substitude [("A", e), ("B", as)] (parse "A->B->A"), Impl as e]
remake m as e (ByAssumption x) = 
	if as /= e then [e, substitude [("A", e), ("B", as)] (parse "A->B->A"), Impl as e]
	else map ((substitude [("A", e)]) . parse) [
																							 "A->(A->A)"
																							,"(A->A->A)->(A->(A->A)->A)->(A->A)"
																							,"(A->(A->A)->A)->(A->A)"
																							,"A->(A->A)->A"
																							,"A->A"
																							]  

remake m as e (ByModusPones j k) = 
	let extract v = 
		case v of 
			Nothing -> (parse "This can't be")
			Just t -> t 
	in map (substitude [("A", as), ("B", extract (M.lookup j m)), ("C", e)] . parse) [
																																											 "(A->B)->((A->(B->C))->(A->C))"
																																											,"((A->(B->C))->(A->C))"
																																											,"A->C"
																																										 ]

deduct :: Exp -> [Exp] -> [Annotation] -> [Exp]
deduct = f' 0 M.empty
	where 
		f' _ _ _ [] [] = []
		f' c m as (e:el) (a:al) = (remake m as e a) ++ (f' (c+1) (M.insert c e m) as el al) 







