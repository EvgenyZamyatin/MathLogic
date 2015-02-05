module Expression where
import qualified Data.Map as H
import Util

data Exp = 
					And Exp Exp 
				| Or Exp Exp 
				| Impl Exp Exp
				| Not Exp
				| Var String
				deriving (Ord, Eq)

instance Show Exp where
	show e = show' 0 e
		where
			show' c t = 
				let openBraket a b = if a > b then "(" else "" in
					let closeBraket a b = if a > b then ")" else "" in
						case t of 
							(And x y) -> (openBraket c 3) ++ (show' 3 x) ++ "&" ++ (show' 3 y) ++ closeBraket c 3
							(Or x y) -> (openBraket c 2) ++ (show' 2 x) ++ "|" ++ (show' 2 y) ++ closeBraket c 2
							(Impl x y) -> 
								case x of 
									(Impl _ _) -> (openBraket c 1) ++ "(" ++ (show' 1 x) ++ ")" ++ "->" ++ (show' 1 y) ++ closeBraket c 1
									_ -> (openBraket c 1) ++ (show' 1 x) ++ "->" ++ (show' 1 y) ++ closeBraket c 1
							(Not x) -> (openBraket c 4) ++ "!" ++ (show' 4 x) ++ closeBraket c 4
							(Var v) -> v
						
match' :: Pair (H.Map String Exp) Bool -> Exp -> Exp -> Pair (H.Map String Exp) Bool
match' p (Impl a1 b1) (Impl a2 b2) = 
	case p of 
		Pair _ False -> p
		Pair _ True -> match' (match' p a1 a2) b1 b2

match' p (Or a1 b1) (Or a2 b2) = 
	case p of 
		Pair _ False -> p
		Pair _ True -> match' (match' p a1 a2) b1 b2

match' p (And a1 b1) (And a2 b2) = 
	case p of 
		Pair _ False -> p
		Pair _ True -> match' (match' p a1 a2) b1 b2

match' p (Not a1) (Not a2) = 
	case p of 
		Pair _ False -> p
		Pair _ True -> (match' p a1 a2)

match' p (Var a1) b1 =
	case fnd of 
		Nothing -> Pair (H.insert a1 b1 (frs p)) (scn p)
		Just t ->  Pair (frs p) (scn p && t == b1)	
		where fnd = H.lookup a1 (frs p)

match' p _ _ = Pair (frs p) False

match :: Exp -> Exp -> Bool --First axiom
match a b = scn (match' (Pair H.empty True) a b)


substitude :: [(String, Exp)] -> Exp -> Exp
substitude list tmpl = f (H.fromList list) tmpl 
	where 
		f m (Var v) = 
			let e = H.lookup v m in 
				case e of 
					Nothing -> Var v
					(Just t) -> t
		f m (Not x) = Not (f m x)
		f m (And x y) = And (f m x) (f m y)
		f m (Or x y) = Or (f m y) (f m y)
		f m (Impl x y) = Impl (f m x) (f m y)