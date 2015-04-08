module OrdinalCalculator where



data Ordinal = O (Ordinal, Int) Ordinal
							|A Int

data Equation = Eq Ordinal Ordinal


str2Ord s = if (s == "w") then O (A 1, 1) (A 0) else (A (read s))

fe (O (o, x) t) = o
fe (A _) = A 0

fc (O (o, x) t) = A x

unc (A x) = x

rst (O _ b) = b
frs (O (o, x) t) = O (o,x) (A 0)

atom (A _) = True
atom _ = False


cmp :: Ordinal -> Ordinal -> Int
cmp (A x) (A y) = x - y
cmp (A _) _ = -1
cmp _ (A _) = 1
cmp a b = if ((cmp (fe a) (fe b)) /= 0) then cmp (fe a) (fe b)
						else if ((cmp (fc a) (fc b)) /= 0) then (cmp (fc a) (fc b))
							else cmp (rst a) (rst b)

addOrd :: Ordinal -> Ordinal -> Ordinal
addOrd (A a) (A b) = A (a+b)
addOrd a b = if ((cmp (fe a) (fe b)) < 0) then b
							else if ((cmp (fe a) (fe b)) == 0) then (O ((fe a), unc (addOrd (fc a) (fc b)) ) (rst b))
								else O (fe a, unc (fc a)) ((addOrd (rst a) b))

subOrd :: Ordinal -> Ordinal -> Ordinal
subOrd (A a) (A b) = A (max 0 (a - b))
subOrd a b = if (cmp (fe a) (fe b) < 0) then (A 0)
							else if (cmp (fe a) (fe b) > 0) then a
								else if (cmp (fc a) (fc b) < 0) then (A 0)
									else if ((cmp (fc a) (fc b) > 0)) then O (fe a, unc (subOrd (fc a) (fc b))) (rst a)
										else subOrd (rst a) (rst b)

mulOrd :: Ordinal -> Ordinal -> Ordinal
mulOrd (A 0) _ = A 0
mulOrd _ (A 0) = A 0
mulOrd (A a) (A b) = A (a * b)
mulOrd a (A b) = O (fe a, (unc (fc a)) * b) (rst a)
mulOrd a b = O (addOrd (fe a) (fe b), unc (fc b)) (mulOrd a (rst b))


pow a (A b) = if (b == 0) then 1 else a * (pow a (A (b-1)))

exp1 :: Int -> Ordinal -> Ordinal
exp1 a b = if ((cmp (fe b) (A 1)) == 0) then O (fc b, (pow a (rst b))) (A 0)
						else if (atom (rst b)) then O ((O ((subOrd (fe b) (A 1)), unc (fc b)) (A 0)), (pow a (rst b))) (A 0)
							else O (subOrd (fe b) (A 1), 1) (O (fe c, unc (fc c)) (A 0))
								where c = exp1 a (rst b)

exp2 :: Ordinal -> Int -> Ordinal
exp2 a 1 = a
exp2 a b = mulOrd (O (mulOrd (fe a) (A (b-1)), 1) (A 0)) a

limitp (A 0) = True
limitp (A _) = False
limitp a = limitp (rst a)

limitpart (A _) = (A 0)
limitpart (O a b) = O a (limitpart b)

natpart (A a) = a
natpart a = natpart (rst a)


firstn a 0 = (A 0)
firstn (O a b) n = O a (firstn b (n-1))

restn a 0 = a
restn (O a b) n = restn (rst b) (n-1)



append (A _) b = b
append (O a b) c = O a (append b c)

padd a b n = append (firstn a n) (addOrd (restn a n) b)

helper a p n 0 = p
helper a p n q = padd (mulOrd (exp2 a q) p) (helper a p n (q-1)) n

len (A _) = 0
len a = 1 + (len (rst a))

exp3 a (A 0) = A 1
exp3 a (A 1) = a
exp3 a (A n) = if (limitp a) then (exp2 a n)
								else mulOrd (exp3 a (A (n-1))) a

exp4 a b = mulOrd (O (mulOrd (fe a) (limitpart b), 1) (A 0)) (exp3 a (A (natpart b)))

expOrd a (A 0) = A 1
expOrd (A 1) b = A 1
expOrd (A a) (A b) = A (pow a (A b))
expOrd (A a) b = exp1 (a) b
expOrd a (A b) = exp3 a (A b)
expOrd a b = exp4 a b




