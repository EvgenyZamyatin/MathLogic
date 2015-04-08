module Task8 where
import Parser
import OrdinalCalculator
--sourse: http://www.ccs.neu.edu/home/pete/pub/cade-algorithms-ordinal-arithmetic.pdf
equals s = 
	let p = (parse s) in
		case p of 
			(O (a, _) (O (b, _) _)) -> if ((cmp a b) == 0) then "Equals" else "Not equals"

main = readFile "task8.in" >>= (return . equals) >>= writeFile "task8.out"
