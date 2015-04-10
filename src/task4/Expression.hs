module Expression where
import qualified Data.Map as H
import Util



data InFile = InFile {assumptions::[Exp]
										 ,statement::Exp
										 ,proof::[Exp]
										 } 

data Exp = 
					And Exp Exp 
				| Or Exp Exp 
				| Impl Exp Exp
				| Not Exp
				| Each String Exp
				| Exist String Exp 
				| SomePredicate String [Term]
				| EqualPredicate Term Term
				| Template String
				deriving (Eq, Ord)

instance Show Exp where
	show e = show' 0 e
		where
			show' c t = 
				let openBraket a b = if a > b then "(" else "" in
					let closeBraket a b = if a > b then ")" else "" in
						case t of 
							(And x y) -> "(" ++ (show' 3 x) ++ ")&(" ++ (show' 3 y) ++ ")"
							(Or x y) -> "(" ++ (show' 2 x) ++ ")|(" ++ (show' 2 y) ++ ")"
							(Impl x y) -> 
								case x of 
									(Impl _ _) -> "(" ++ (show' 1 x) ++ ")->(" ++ (show' 1 y) ++ ")"
									_ -> "(" ++ (show' 1 x) ++ ")->(" ++ (show' 1 y) ++")"
							(Not x) -> "!(" ++ (show' 4 x) ++ ")"
							(Each x e) -> "@"++x++"("++(show' 0 e)++")"
							(Exist x e) -> "?"++x++"("++(show' 0 e)++")"
							(SomePredicate x tl) -> "(" ++ x ++(show tl) ++ ")"
							(EqualPredicate a b) -> "("++(show a) ++ ")=(" ++ (show b) ++ ")"



data Term = 
					Sum Term Term
				|	Mul Term Term
				| Fun String [Term]
				| Zero
				| Var String
				| Nxt Term
				deriving (Eq, Ord)
instance Show Term where
	show (Sum a b) =  "("++(show a)++")" ++ "+" ++ "(" ++(show b)++")" 
	show (Mul a b) = "("++(show a)++")" ++ "*" ++ "(" ++(show b)++")" 
	show (Fun f tl) = "(" ++ f ++ (show tl) ++ ")"
	show Zero = "0"
	show (Var x) = x
	show (Nxt x) = "(" ++ (show x) ++ ")" ++ "'"

				
appendAssumption :: InFile -> Exp -> InFile
appendAssumption f a = InFile (a:(assumptions f)) (statement f) (proof f)

substitude :: [(String, Exp)] -> Exp -> Exp
substitude list tmpl = f (H.fromList list) tmpl 
	where 
		f m (Template v) = 
			let e = H.lookup v m in 
				case e of 
					(Just t) -> t
		f m (Not x) = Not (f m x)
		f m (And x y) = And (f m x) (f m y)
		f m (Or x y) = Or (f m x) (f m y)
		f m (Impl x y) = Impl (f m x) (f m y)
		f m (Each x y) = Each x (f m y)
		f m (Exist x y) = Exist x (f m y)
		f m (EqualPredicate x y) = (EqualPredicate x y)







