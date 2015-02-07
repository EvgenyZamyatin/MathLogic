module Proover where

import Verifier
import Parser
import Expression
import Util
import Deductor
import qualified Axioms as A
import Lemmas

proov :: Exp -> [Exp]
proov e = proov' [] e
	where 
		proov' as (Impl a b) = deductLast (a:as) (proov' (a:as) b)
		proov' as (And a b) = (proov' as a) ++ (proov' as b) ++ (map ((substitude [("A", a), ("B", b)]). parse) ["A->B->A&B", "B->A&B", "A&B"])
		proov' as (Or a b) = (proov' as newImpl) ++ (implToOr newImpl) ++ (map ((substitude [("A", a), ("B", b)]). parse) ["A|B"])
			where newImpl = (substitude [("A", a), ("B", b)] (parse "!A->B"))  
		proov' as (Not (And a b)) = (proov' as newImpl) ++ (implToNotAnd newImpl) ++ (map ((substitude [("A", a), ("B", b)]). parse) ["!(A&B)"])
			where newImpl = (substitude [("A", a), ("B", b)] (parse "A->!B"))
		
		proov' as (Not (Impl a b)) = (proov' as newAnd) ++ (andToNotImpl newAnd) ++ (map ((substitude [("A", a), ("B", b)]). parse) ["!(A->B)"])
			where newAnd = (substitude [("A", a), ("B", b)] (parse "A&!B"))
		proov' as (Not (Or a b)) = (proov' as (Not a)) ++ (proov' as (Not b)) 
																++ (map ((substitude [("A", a), ("B", b)]). parse) ["!A->!B->!A&!B", "!B->!A&!B", "!A&!B"])
																++ (andToNotOr (And (Not a) (Not b))) ++ (map ((substitude [("A", a), ("B", b)]). parse) ["!(A|B)"])
		proov' as (Var a) = [(Var a)]
		proov' as (Not (Var a)) = [Not (Var a)]
		proov' as (Not (Not a)) = (proov' as a) ++ (notNot (Not (Not a))) ++ [parse "!!A"]     

		
