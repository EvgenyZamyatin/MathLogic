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
