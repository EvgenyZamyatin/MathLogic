module Lemmas where
import Expression
import Parser

subAtA1 :: Term -> Term -> [Exp]
subAtA1 a b = 
	let tmp = Impl (EqualPredicate a (Var "b")) (EqualPredicate (Nxt a) (Nxt (Var "b"))) in 
	let a' = (Impl (EqualPredicate a b) (EqualPredicate (Nxt a) (Nxt b))) in
				map ((substitude  [("A", tmp), ("B", a'), ("F", parse "a=b->a'=b'"), ("E", parse "P->P->P")]) . parse) 
				[ "#F"
				, "#E"
				, "#F->#E->#F"
				, "#E->#F"
				, "#E->@a(#F)"
				, "@a(#F)"
				, "@a(#F)->#A"
				, "#A"
				, "#A->#E->#A"
				, "#E->#A"
				, "#E->@b(#A)"
				, "@b(#A)"
				, "@b(#A)->#B"
				, "#B"
				]

subAtA2 :: Term->Term->Term->[Exp]
subAtA2 a b c = 
	let tmp1 = Impl (EqualPredicate a (Var "b")) (Impl (EqualPredicate a (Var "c")) (EqualPredicate (Var "b") (Var "c"))) in 
	let tmp2 = Impl (EqualPredicate a b) (Impl (EqualPredicate a (Var "c")) (EqualPredicate b (Var "c"))) in 
	let a' = (Impl (EqualPredicate a b) (Impl (EqualPredicate a c) (EqualPredicate b c))) in
				map ((substitude  [("A", tmp1), ("B", tmp2), ("C", a'), ("F", parse "a=b->a=c->b=c"), ("E", parse "P->P->P")]) . parse) 
				[ "#F"
				, "#E"
				, "#F->#E->#F"
				, "#E->#F"
				, "#E->@a(#F)"
				, "@a(#F)"
				, "@a(#F)->#A"
				, "#A"
				, "#A->#E->#A"
				, "#E->#A"
				, "#E->@b(#A)"
				, "@b(#A)"
				, "@b(#A)->#B"
				, "#B"
				, "#B->#E->#B"
				, "#E->#B"
				, "#E->@c(#B)"
				, "@c(#B)"
				, "@c(#B)->#C"
				, "#C"
				]

subAtA5 :: Term->Term->[Exp]
subAtA5 a b = 
	let tmp = (EqualPredicate (Sum a (Nxt (Var "b"))) (Nxt (Sum a (Var "b")))) in 
	let a' = (EqualPredicate (Sum a (Nxt b)) (Nxt (Sum a b))) in
				map ((substitude  [("A", tmp), ("B", a'), ("F", parse "a+b'=(a+b)'"), ("E", parse "P->P->P")]) . parse) 
				[ "#F"
				, "#E"
				, "#F->#E->#F"
				, "#E->#F"
				, "#E->@a(#F)"
				, "@a(#F)"
				, "@a(#F)->#A"
				, "#A"
				, "#A->#E->#A"
				, "#E->#A"
				, "#E->@b(#A)"
				, "@b(#A)"
				, "@b(#A)->#B"
				, "#B"
				]


-- a is like (Term + 0 = Term)
subAtA6 :: Term -> [Exp]
subAtA6 a = 
	map ((substitude  [("T", EqualPredicate (Sum a Zero) a), ("E", parse "P->P->P")]) . parse) 
	[ "a+0=a"
	, "#E"
	, "a+0=a->#E->a+0=a"
	, "#E->a+0=a"
	, "#E->@a(a+0=a)"
	, "@a(a+0=a)"
	, "@a(a+0=a)->#T"
	, "#T"
	]

-- a is like (Term * 0 = 0)
subAtA7 :: Term -> [Exp]
subAtA7 a = 
	map ((substitude  [("T", EqualPredicate (Mul a Zero) Zero), ("E", parse "P->P->P")]) . parse) 
	[ "a*0=0"
	, "#E"
	, "a*0=0->#E->a*0=0"
	, "#E->a*0=0"
	, "#E->@a(a*0=0)"
	, "@a(a*0=0)"
	, "@a(a*0=0)->#T"
	, "#T"
	]


-- a is like (a * b' = a*b + a)
subAtA8 :: Term->Term -> [Exp]
subAtA8 a b = 
	let tmp = EqualPredicate (Mul a (Nxt (Var "b"))) (Sum (Mul a (Var "b")) a) in 
	let a' = (EqualPredicate (Mul a (Nxt b)) (Sum (Mul a b) a)) in
				map ((substitude  [("A", tmp), ("B", a'), ("E", parse "P->P->P")]) . parse) 
				[ "a*b'=a*b+a"
				, "#E"
				, "a*b'=a*b+a->#E->a*b'=a*b+a"
				, "#E->a*b'=a*b+a"
				, "#E->@a(a*b'=a*b+a)"
				, "@a(a*b'=a*b+a)"
				, "@a(a*b'=a*b+a)->#A"
				, "#A"
				, "#A->#E->#A"
				, "#E->#A"
				, "#E->@b(#A)"
				, "@b(#A)"
				, "@b(#A)->#B"
				, "#B"
				]


aEa a = 
	let tmp = (EqualPredicate (Sum a Zero) a) in 
		(subAtA6 a) ++ (subAtA2 (Sum a Zero) a a) ++ [Impl tmp (EqualPredicate a a), (EqualPredicate a a)]


reflection t r = 
	let tmp = (EqualPredicate t r) in
		[(EqualPredicate t r)] ++ 
		(aEa t) ++ 
		(subAtA2 t r t) ++
		[(Impl (EqualPredicate t t) (EqualPredicate r t))] ++
		[(EqualPredicate r t)]

addZero a b = 
	[EqualPredicate a b] ++
	(subAtA6 a) ++
	(reflection (Sum a Zero) a) ++
	(subAtA2 a (Sum a Zero) b) ++
	[Impl (EqualPredicate a b) (EqualPredicate (Sum a Zero) b)] ++ 
	[(EqualPredicate (Sum a Zero) b)]


{-
subAtA1 :: Exp -> [Exp]
subAtA1 (Impl (EqualPredicate a b) (EqualPredicate c d)) = 
	let tmp = Impl (EqualPredicate a (Var "b")) (EqualPredicate c (Nxt (Var "b"))) in 
	let a' = (Impl (EqualPredicate a b) (EqualPredicate c d)) in
				map ((substitude  [("A", tmp), ("B", a'), ("F", parse "a=b->a'=b'"), ("E", parse "P->P->P")]) . parse) 
				[ "#F"
				, "#E"
				, "#F->#E->#F"
				, "#E->#F"
				, "#E->@a(#F)"
				, "@a(#F)"
				, "@a(#F)->#A"
				, "#A"
				, "#A->#E->#A"
				, "#E->#A"
				, "#E->@b(#A)"
				, "@b(#A)"
				, "@b(#A)->#B"
				, "#B"
				]

subAtA2 :: Exp -> [Exp]
subAtA2 (Impl (EqualPredicate a b) (Impl (EqualPredicate c d) (EqualPredicate e f))) = 
	let tmp1 = Impl (EqualPredicate a (Var "b")) (Impl (EqualPredicate c (Var "c")) (EqualPredicate (Var "b") (Var "c"))) in 
	let tmp2 = Impl (EqualPredicate a b) (Impl (EqualPredicate c (Var "c")) (EqualPredicate b (Var "c"))) in 
	let a' = (Impl (EqualPredicate a b) (Impl (EqualPredicate c d) (EqualPredicate e f))) in
				map ((substitude  [("A", tmp1), ("B", tmp2), ("C", a'), ("F", parse "a=b->a=c->b=c"), ("E", parse "P->P->P")]) . parse) 
				[ "#F"
				, "#E"
				, "#F->#E->#F"
				, "#E->#F"
				, "#E->@a(#F)"
				, "@a(#F)"
				, "@a(#F)->#A"
				, "#A"
				, "#A->#E->#A"
				, "#E->#A"
				, "#E->@b(#A)"
				, "@b(#A)"
				, "@b(#A)->#B"
				, "#B"
				, "#B->#E->#B"
				, "#E->#B"
				, "#E->@c(#B)"
				, "@c(#B)"
				, "@c(#B)->#C"
				, "#C"
				]


subAtA5 :: Exp -> [Exp]
subAtA5 (EqualPredicate (Sum a (Nxt b)) (Nxt (Sum c d))) = 
	let tmp = (EqualPredicate (Sum a (Nxt (Var "b"))) (Nxt (Sum a (Var "b")))) in 
	let a' = (EqualPredicate (Sum a (Nxt b)) (Nxt (Sum a b))) in
				map ((substitude  [("A", tmp), ("B", a'), ("F", parse "a+b'=(a+b)'"), ("E", parse "P->P->P")]) . parse) 
				[ "#F"
				, "#E"
				, "#F->#E->#F"
				, "#E->#F"
				, "#E->@a(#F)"
				, "@a(#F)"
				, "@a(#F)->#A"
				, "#A"
				, "#A->#E->#A"
				, "#E->#A"
				, "#E->@b(#A)"
				, "@b(#A)"
				, "@b(#A)->#B"
				, "#B"
				]


-- a is like (Term + 0 = Term)
subAtA6 :: Exp -> [Exp]
subAtA6 a = 
	map ((substitude  [("T", a), ("E", parse "P->P->P")]) . parse) 
	[ "a+0=a"
	, "#E"
	, "a+0=a->#E->a+0=a"
	, "#E->a+0=a"
	, "#E->@a(a+0=a)"
	, "@a(a+0=a)"
	, "@a(a+0=a)->#T"
	, "#T"
	]

-- a is like (Term * 0 = 0)
subAtA7 :: Exp -> [Exp]
subAtA7 a = 
	map ((substitude  [("T", a), ("E", parse "P->P->P")]) . parse) 
	[ "a*0=0"
	, "#E"
	, "a*0=0->#E->a*0=0"
	, "#E->a*0=0"
	, "#E->@a(a*0=0)"
	, "@a(a*0=0)"
	, "@a(a*0=0)->#T"
	, "#T"
	]

-- a is like (a * b' = a*b + a)
subAtA8 :: Exp -> [Exp]
subAtA8 (EqualPredicate (Mul a b) (Sum (Mul c d) e)) = 
	let tmp = EqualPredicate (Mul a (Nxt (Var "b"))) (Sum (Mul c (Var "b")) e) in 
	let a' = (EqualPredicate (Mul a b) (Sum (Mul c d) e)) in
				map ((substitude  [("A", tmp), ("B", a'), ("E", parse "P->P->P")]) . parse) 
				[ "a*b'=a*b+a"
				, "#E"
				, "a*b'=a*b+a->#E->a*b'=a*b+a"
				, "#E->a*b'=a*b+a"
				, "#E->@a(a*b'=a*b+a)"
				, "@a(a*b'=a*b+a)"
				, "@a(a*b'=a*b+a)->#A"
				, "#A"
				, "#A->#E->#A"
				, "#E->#A"
				, "#E->@b(#A)"
				, "@b(#A)"
				, "@b(#A)->#B"
				, "#B"
				]


aEa (EqualPredicate a b) = 
	let tmp = (EqualPredicate (Sum a Zero) a) in 
		(subAtA6 tmp) ++ (subAtA2 (Impl tmp (Impl tmp (EqualPredicate a a)))) ++ [Impl tmp (EqualPredicate a a), (EqualPredicate a a)]


reflection (EqualPredicate t r) = 
	let tmp = (EqualPredicate t r) in
		[(EqualPredicate t r)] ++ 
		aEa (EqualPredicate t t) ++ 
		(subAtA2 (Impl tmp (Impl (EqualPredicate t t) (EqualPredicate r t)))) ++
		[(Impl (EqualPredicate t t) (EqualPredicate r t))] ++
		[(EqualPredicate r t)]
-}