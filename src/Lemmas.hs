module Lemmas where

import Deductor
import Verifier
import Parser
import Util
import Axioms
import Expression

deMorgan (Not (Or a b)) = 
	(deductAll [parse "!(A|B)"] ((map parse ["B->(A|B)", "A->(A|B)"]) 
	++ (contraposition (parse "A->(A|B)")) 
	++ (contraposition (parse "B->(A|B)")) 
	++ (map parse ["!(A|B)->!B", "!(A|B)->!A", "!(A|B)", "!A", "!B", "!A->!B->!A&!B", "!B->!A&!B", "!A&!B"])))

contraposition (Impl a b) = map ((substitude [("A", a), ("B", b)])) exps
	where  
		exps = deductAll (map parse ["A->B", "!B"]) (map parse list)
			where list = ["(A->B)->(A->!B)->!A"
										,"A->B"
										,"(A->!B)->!A"
										,"!B->A->!B"
										,"!B"
										,"A->!B"
										,"!A"]

aToa e = map (substitude [("A", e)]) (map parse list) 
	where list =  ["A->(A->A)"
								,"(A->A->A)->(A->(A->A)->A)->(A->A)"
								,"(A->(A->A)->A)->(A->A)"
								,"A->(A->A)->A"
								,"A->A"
								]

intuit1 a b = map (substitude [("A", a), ("B", b)]) (deductAll (map parse ["A", "!A"]) (map parse ["A", "!A", "A->!B->A", "!B->A", "!A->!B->!A", "!B->!A",
																																																	 "(!B->A)->(!B->!A)->!!B", "(!B->!A)->!!B", "!!B",
																																																	 "!!B->B", "B"])) 
intuit2 a b = map (substitude [("A", a), ("B", b)]) (deductAll (map parse ["!A", "A"]) (map parse ["A", "!A", "A->!B->A", "!B->A", "!A->!B->!A", "!B->!A",
																																																	 "(!B->A)->(!B->!A)->!!B", "(!B->!A)->!!B", "!!B",
																																																	 "!!B->B", "B"])) 

implToOr (Impl (Not a) b) = map (substitude [("A", a), ("B", b)]) exps
	where
		exps = deductAll as 
				([parse "(!(A|B)->B)->(!(A|B)->!B)->!!(A|B)"]
			++
			(deductLast ((parse "!(A|B)"):as) (
			[parse "!(A|B)"]
			++((deMorgan . parse) "!(A|B)")
			++[parse "!A&!B"
				,parse "!A&!B->!A"
				,parse "!A"
				,parse "!A->B"
				,parse "B"
			  ]))
			++[parse "(!(A|B)->!B)->!!(A|B)"]
			++((contraposition . parse) "B->(A|B)")
			++[parse "B->(A|B)"]
			++[parse "!(A|B)->!B"]
			++[parse "!!(A|B)"]
			++[parse "!!(A|B)->(A|B)"]
			++[parse "A|B"])
				where as = [(parse "!A->B")] 

implToNotAnd (Impl a (Not b)) = map (substitude [("A", a), ("B", b)]) exps
	where 
		exps = deductAll as (
				[parse "(A&B->B)->((A&B)->!B)->!(A&B)"]
			++[parse "A&B->B"]
			++[parse "((A&B)->!B)->!(A&B)"]
			++(deductLast ((parse "A&B"):as) (map parse ["A&B", "A&B->A", "A->!B", "A", "!B"]))
			++[parse "!(A&B)"])
				where as = [parse "A->!B"]

andToNotImpl (And a (Not b)) = map (substitude [("A", a), ("B", b)]) exps
	where
		exps = deductAll as (
				[parse "((A->B)->(B->(A->B)))->((A->B)->!(B->(A->B)))->!(A->B)"]
			++(deductLast ((parse "A->B"):as) (map parse ["A->B", "A&!B->A", "A&!B", "A", "B", "A&!B->!B", "!B", 
																										"B->((B->A->B))->B", "!B->((B->A->B))->!B", "((B->A->B))->B", "((B->A->B))->!B",
																										"(((B->A->B))->B)->(((B->A->B))->!B)->!((B->A->B))", "(((B->A->B))->!B)->!((B->A->B))", 
																										"!((B->A->B))"]))			
			++[parse "((A->B)->(B->(A->B)))"]
			++[parse "((A->B)->!(B->(A->B)))->!(A->B)"]
			++[parse "!(A->B)"]
			)
			where as = [parse "A&!B"]   

andToNotOr (And (Not a) (Not b)) = map (substitude [("A", a), ("B", b)]) exps
	where 
		exps = deductAll as (
				[parse "(A->B)->(B->B)->(A|B->B)"]
			++(intuit2 (Var "A") (Var "B"))
			++[parse "!A&!B->!A"]
			++[parse "!A&!B"]
			++[parse "!A"]
			++[parse "A->B"]	
			++[parse "(B->B)->(A|B->B)"]
			++(aToa (Var "B"))
			++[parse "(A|B->B)"]
			++(contraposition (parse "(A|B->B)"))
			++[parse "!B->!(A|B)"]
			++[parse "!A&!B->!B"]
			++[parse "!B"]
			++[parse "!(A|B)"]
			)
			where as = [parse "!A&!B"]
notNot (Not (Not a)) = map (substitude [("A", a)]) exps
	where 
		exps = deductAll as (
				[parse "(!A->A)->(!A->!A)->!!A"]
			++(deductLast ((parse "!A"):as) (map parse ["A"]))
			++[parse "(!A->!A)->!!A"]
			++(aToa (Var "!A"))
			++[parse "!!A"]
			)
			where as = [parse "A"] 