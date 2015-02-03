module Expression where
import qualified Data.Map as H
import Util

data Exp = 
					And Exp Exp 
				| Or Exp Exp 
				| Impl Exp Exp
				| Not Exp
				| Var String
				deriving (Show, Ord, Eq)

match' :: Pair (H.Map String Exp) Bool -> Exp -> Exp -> Pair (H.Map String Exp) Bool
match' p (Impl a1 b1) (Impl a2 b2) = match' (match' p a1 a2) b1 b2
match' p (Or a1 b1) (Or a2 b2) = match' (match' p a1 a2) b1 b2
match' p (And a1 b1) (And a2 b2) = match' (match' p a1 a2) b1 b2
match' p (Not a1) (Not a2) = (match' p a1 a2)
match' p (Var a1) b1 =
	case fnd of 
		Nothing -> Pair (H.insert a1 b1 (frs p)) (scn p)
		Just t ->  Pair (frs p) (scn p && t == b1)	
		where fnd = H.lookup a1 (frs p)
match' p _ _ = Pair (frs p) False

match :: Exp -> Exp -> Bool --First axiom
match a b = scn (match' (Pair H.empty True) a b)
