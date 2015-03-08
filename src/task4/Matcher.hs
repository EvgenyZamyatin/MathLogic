module Matcher where
import Expression
import Parser
import qualified Data.Map as H
import qualified Data.Set as S


data Error = 
						NoError 
					| Error1 Term String
					| Error2 String Exp 
					| Error3 String Exp Int
					| SomeError deriving (Eq)
instance Show Error where
	show (Error1 a b) = "Терм " ++ (show a) ++ " не свободен для подстановки в формулу вместо переменной " ++ (show b)
	show (Error2 a b) = "Переменная " ++ a ++ " входит свободно в формулу " ++ (show b)
	show SomeError = "Некорректный ввод"
	show (Error3 x e i) = "Используется правило или схема аксиом с квантером по переменной " ++ x ++ ", входящей свободно в допущение " ++ (show e)


match :: Exp -> Exp -> Error --First axiom
match a b = 
	let (f, s) = (match' H.empty S.empty a b)
		in s

match' :: H.Map String String -> S.Set String -> Exp -> Exp -> (H.Map String String, Error)
match' mc bind (Impl a1 b1) (Impl a2 b2) = 
		let (nmc, f) = match' mc bind a1 a2 
			in case f of
				NoError -> match' nmc bind b1 b2
				otherwise -> (mc, f)

match' mc bind (Or a1 b1) (Or a2 b2) = 
		let (nmc, f) = match' mc bind a1 a2 
			in case f of
				NoError -> match' nmc bind b1 b2
				otherwise -> (mc, f)
				
match' mc bind (And a1 b1) (And a2 b2) = 
		let (nmc, f) = match' mc bind a1 a2 
			in case f of
				NoError -> match' nmc bind b1 b2
				otherwise -> (mc, f)
				
match' mc bind (Not a1) (Not a2) = 
		let (nmc, f) = match' mc bind a1 a2 
			in (nmc, f)

match' mc bind (Template a1) b1 =
	case fnd of 
		Nothing -> (H.insert a1 (show b1) mc, NoError)
		Just t ->  (mc, if (t == (show b1)) then NoError else SomeError)	
		where fnd = H.lookup a1 mc

match' mc bind (Each a f1) (Each b f2) = 
	case (a == b) of 
		True -> match' mc (S.insert a bind) f1 f2
		False -> (mc, SomeError)

match' mc bind (Exist a f1) (Exist b f2) = 
	case (a == b) of 
		True -> match' mc (S.insert a bind) f1 f2
		False -> (mc, SomeError)

match' mc bind (SomePredicate p1 tl1) (SomePredicate p2 tl2) = 
	case p1 == p2 of 
		True -> matchTermList mc bind tl1 tl2
		False -> (mc, SomeError)

match' mc bind (EqualPredicate a1 a2) (EqualPredicate b1 b2) = 
	matchTermList mc bind [a1, a2] [b1, b2]

match' mc _ _ _ = (mc, SomeError)



matchTermList :: H.Map String String -> S.Set String -> [Term] -> [Term] -> (H.Map String String, Error)
matchTermList mc bind (a:xa) (b:xb) = 
	case (matchTerm mc bind a b) of
		(a, NoError) -> matchTermList a bind xa xb
		(a, e) -> (a, e)
matchTermList p bind ([]) ([]) = (p, NoError)
matchTermList p bind (xa) ([]) = (p, SomeError)
matchTermList p bind ([]) (xb) = (p, SomeError)

matchTerm :: H.Map String String -> S.Set String -> Term -> Term -> (H.Map String String, Error)
matchTerm mc bind (Sum a1 b1) (Sum a2 b2) = 
	let (f, s) = matchTerm mc bind a1 a2
		in case s of 
			NoError -> matchTerm mc bind b1 b2
			otherwise -> (mc, s)
			
matchTerm mc bind (Mul a1 b1) (Mul a2 b2) = 
	let (f, s) = matchTerm mc bind a1 a2
		in case s of 
			NoError  -> matchTerm mc bind b1 b2
			otherwise -> (mc, s)

matchTerm mc bind (Fun f1 tl1) (Fun f2 tl2) = 
	case f1 == f2 of 
			True  -> matchTermList mc bind tl1 tl2
			False -> (mc, SomeError)
			
matchTerm mc bind Zero Zero = 
	(mc, NoError)

matchTerm mc bind (Nxt a) (Nxt b) = 
	matchTerm mc bind a b

matchTerm mc bind (Var a) b = if (Var a) == b then (mc, NoError) else
	case S.member a bind of 
		True  -> (mc, if (Var a) == b then NoError else SomeError)
		False -> case H.lookup a mc of
						 	 Nothing -> if (canSubsitude bind (Var a) b == NoError) then (H.insert a (show b) mc, NoError) else (mc, canSubsitude bind (Var a) b) 
						 	 Just t  -> if (canSubsitude bind (Var a) b == NoError && t == (show b)) then (mc, NoError) else (mc, SomeError) 
matchTerm mc bind _ _ =
	(mc, SomeError)

canSubsitude :: S.Set String -> Term -> Term -> Error
canSubsitude binded (Var x) t = 
	foldl 
	(\start -> \x -> (if (start /= NoError) then start else (if (S.member x binded) then Error1 t x else NoError)))
	NoError (varList t)

varList t = 
	case t of 
		(Sum a b) -> (varList a) ++ (varList b) 
		(Mul a b) -> (varList a) ++ (varList b) 
		(Fun a b) -> foldl (++) [] (map varList b)
		(Zero)    -> []
		(Var a)   -> [a]
		(Nxt a)   -> varList a	


isFree :: String -> Exp -> Bool
isFree x (Impl a b) = (isFree x a) || (isFree x b) 
isFree x (Or a b) = (isFree x a) || (isFree x b) 
isFree x (And a b) = (isFree x a) || (isFree x b) 
isFree x (Not a) = (isFree x a)
isFree x (Each s a) = if (x==s) then False else (isFree x a)
isFree x (Exist s a) = if (x==s) then False else (isFree x a)
isFree x (SomePredicate s tl) = foldl (\a -> \b -> (isFree' x b) || a) False tl
isFree x (EqualPredicate t1 t2) = (isFree' x t1) || (isFree' x t2)

isFree' :: String -> Term -> Bool
isFree' x (Sum a b) = (isFree' x a) || (isFree' x b) 
isFree' x (Mul a b) = (isFree' x a) || (isFree' x b) 
isFree' x (Fun a tl) = foldl (\a -> \b -> (isFree' x b) || a) False tl
isFree' x (Zero) = False
isFree' x (Var s) = x == s
isFree' x (Nxt a) = isFree' x a







