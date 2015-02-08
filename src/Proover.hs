module Proover where

import Verifier
import Parser
import Expression
import Util
import Deductor
import qualified Axioms as A
import qualified Data.Map as M
import Lemmas
import qualified Data.List as L
data Proof = Ok [Exp] | Fail [Exp]

instance Show Proof where
	show d = case d of 
		Ok e -> toString e 
			where 
				toString [] = ""
				toString (e:xs) = (show e) ++ "\n" ++ (toString xs)
		(Fail e) -> toStringFail e
			where 
				toStringFail [] = ""
				toStringFail [(Not (Var s))] = s ++ "=Л"
				toStringFail [((Var s))] = s ++ "=И"
				toStringFail (v:vs) = (toStringFail [v]) ++ "," ++ (toStringFail vs)



extract v m = 
	case M.lookup v m of 
		Nothing -> Var "h"
		Just x -> x

letProov :: [Exp] -> Exp -> (Exp, [Exp])
letProov l e = letProov' (convert l) e
	where 
		convert [] = M.empty
		convert (a:as) = 
			case a of 
				(Not (Var s)) -> M.insert s a (convert as)
				(Var s) -> M.insert s a (convert as)

letProov' :: M.Map String Exp ->Exp->(Exp, [Exp])
letProov' as e = 
	case e of 
		And a b -> 
			let ((rf, f),(rs, s)) = (letProov' as a, letProov' as b) 
				in let r = f ++ s ++ (andLem rf rs)
					in (last r, r)  
		Or a b -> 
			let ((rf, f),(rs, s)) = (letProov' as a, letProov' as b) 
				in let r = f ++ s ++ (orLem rf rs)
					in (last r, r)  
		Impl a b -> 
			let ((rf, f),(rs, s)) = (letProov' as a, letProov' as b) 
				in let r = f ++ s ++ (impLem rf rs)
					in (last r, r)  
		Var a -> (extract a as, [])
		Not a -> 
			let (r,d) = letProov' as a 
				in let r' = d ++ (notLem r)
					in (last r', r')


merge :: [String] -> [String] -> [String]
merge a [] = a
merge a (x:b) = 
	case (L.find (\t->t==x) a) of 
		Just r -> (merge a b)
		Nothing -> (x:(merge a b))
	  
findAllVars :: Exp -> [String]
findAllVars (Var s) = [s]
findAllVars (And a b) = merge (findAllVars a) (findAllVars b)
findAllVars (Or a b) = merge (findAllVars a) (findAllVars b)
findAllVars (Impl a b) = merge (findAllVars a) (findAllVars b)
findAllVars (Not a) = findAllVars a



findProof :: [String] -> [Exp] -> Exp -> Proof
findProof [] as e = 
	case (letProov as e) of 
		(a, b) -> if a == e then Ok b else Fail as

findProof (v:vars) as e = 
	let (a, b) = (findProof vars ((Var v):as) e, findProof vars ((Not (Var v)):as) e)
		in case (a, b) of 
			(Fail a, _) -> Fail a
			(_, Fail a) -> Fail a
			(Ok a, Ok b) -> 
					Ok ((deductLast ((Var v):as) a)
					++ (deductLast ((Not (Var v)):as) b) 
					++ (aOrNotA (Var v)) 
					++ ((map ((substitude [("A", e), ("p", Var v)]) . parse) ) ["(p->A)->(!p->A)->(p|!p->A)", "(!p->A)->(p|!p->A)", "(p|!p->A)", "A"]))


{-
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

-}		
