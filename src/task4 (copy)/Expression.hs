module Expression where
import Util



data InFile = InFile {assumptions::[Exp]
										 ,statement::Exp
										 ,proof::[Exp]
										 } 
										 deriving (Show)

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
				deriving (Show, Eq, Ord)

data Term = 
					Sum Term Term
				|	Mul Term Term
				| Fun String [Term]
				| Zero
				| Var String
				| Nxt Term
				deriving (Show, Eq, Ord)
				
appendAssumption :: InFile -> Exp -> InFile
appendAssumption f a = InFile (a:(assumptions f)) (statement f) (proof f)








