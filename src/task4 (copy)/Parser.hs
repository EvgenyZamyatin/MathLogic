{-# OPTIONS_GHC -w #-}
module Parser where
import Data.Char
import Prelude
import Expression

-- parser produced by Happy Version 1.19.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12

action_0 (13) = happyShift action_9
action_0 (14) = happyShift action_10
action_0 (15) = happyShift action_11
action_0 (18) = happyShift action_12
action_0 (20) = happyShift action_13
action_0 (23) = happyShift action_14
action_0 (24) = happyShift action_15
action_0 (27) = happyShift action_16
action_0 (4) = happyGoto action_17
action_0 (5) = happyGoto action_18
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 (10) = happyGoto action_6
action_0 (11) = happyGoto action_7
action_0 (12) = happyGoto action_8
action_0 _ = happyFail

action_1 (13) = happyShift action_9
action_1 (14) = happyShift action_10
action_1 (15) = happyShift action_11
action_1 (18) = happyShift action_12
action_1 (20) = happyShift action_13
action_1 (23) = happyShift action_14
action_1 (24) = happyShift action_15
action_1 (27) = happyShift action_16
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 (8) = happyGoto action_5
action_1 (10) = happyGoto action_6
action_1 (11) = happyGoto action_7
action_1 (12) = happyGoto action_8
action_1 _ = happyFail

action_2 (17) = happyShift action_19
action_2 _ = happyFail

action_3 (16) = happyShift action_32
action_3 _ = happyReduce_3

action_4 _ = happyReduce_5

action_5 _ = happyReduce_7

action_6 (25) = happyShift action_30
action_6 (29) = happyShift action_31
action_6 _ = happyFail

action_7 (26) = happyShift action_29
action_7 _ = happyReduce_18

action_8 (28) = happyShift action_28
action_8 _ = happyReduce_20

action_9 (18) = happyShift action_27
action_9 _ = happyReduce_22

action_10 (18) = happyShift action_26
action_10 _ = happyReduce_15

action_11 _ = happyReduce_12

action_12 (13) = happyShift action_9
action_12 (14) = happyShift action_10
action_12 (15) = happyShift action_11
action_12 (18) = happyShift action_12
action_12 (20) = happyShift action_13
action_12 (23) = happyShift action_14
action_12 (24) = happyShift action_15
action_12 (27) = happyShift action_16
action_12 (4) = happyGoto action_24
action_12 (5) = happyGoto action_18
action_12 (6) = happyGoto action_3
action_12 (7) = happyGoto action_4
action_12 (8) = happyGoto action_5
action_12 (10) = happyGoto action_25
action_12 (11) = happyGoto action_7
action_12 (12) = happyGoto action_8
action_12 _ = happyFail

action_13 (13) = happyShift action_9
action_13 (14) = happyShift action_10
action_13 (15) = happyShift action_11
action_13 (18) = happyShift action_12
action_13 (20) = happyShift action_13
action_13 (23) = happyShift action_14
action_13 (24) = happyShift action_15
action_13 (27) = happyShift action_16
action_13 (7) = happyGoto action_23
action_13 (8) = happyGoto action_5
action_13 (10) = happyGoto action_6
action_13 (11) = happyGoto action_7
action_13 (12) = happyGoto action_8
action_13 _ = happyFail

action_14 (13) = happyShift action_22
action_14 _ = happyFail

action_15 (13) = happyShift action_21
action_15 _ = happyFail

action_16 _ = happyReduce_24

action_17 (30) = happyAccept
action_17 _ = happyFail

action_18 (17) = happyShift action_19
action_18 (21) = happyShift action_20
action_18 _ = happyReduce_1

action_19 (13) = happyShift action_9
action_19 (14) = happyShift action_10
action_19 (15) = happyShift action_11
action_19 (18) = happyShift action_12
action_19 (20) = happyShift action_13
action_19 (23) = happyShift action_14
action_19 (24) = happyShift action_15
action_19 (27) = happyShift action_16
action_19 (6) = happyGoto action_46
action_19 (7) = happyGoto action_4
action_19 (8) = happyGoto action_5
action_19 (10) = happyGoto action_6
action_19 (11) = happyGoto action_7
action_19 (12) = happyGoto action_8
action_19 _ = happyFail

action_20 (13) = happyShift action_9
action_20 (14) = happyShift action_10
action_20 (15) = happyShift action_11
action_20 (18) = happyShift action_12
action_20 (20) = happyShift action_13
action_20 (23) = happyShift action_14
action_20 (24) = happyShift action_15
action_20 (27) = happyShift action_16
action_20 (4) = happyGoto action_45
action_20 (5) = happyGoto action_18
action_20 (6) = happyGoto action_3
action_20 (7) = happyGoto action_4
action_20 (8) = happyGoto action_5
action_20 (10) = happyGoto action_6
action_20 (11) = happyGoto action_7
action_20 (12) = happyGoto action_8
action_20 _ = happyFail

action_21 (13) = happyShift action_9
action_21 (14) = happyShift action_10
action_21 (15) = happyShift action_11
action_21 (18) = happyShift action_12
action_21 (20) = happyShift action_13
action_21 (23) = happyShift action_14
action_21 (24) = happyShift action_15
action_21 (27) = happyShift action_16
action_21 (7) = happyGoto action_44
action_21 (8) = happyGoto action_5
action_21 (10) = happyGoto action_6
action_21 (11) = happyGoto action_7
action_21 (12) = happyGoto action_8
action_21 _ = happyFail

action_22 (13) = happyShift action_9
action_22 (14) = happyShift action_10
action_22 (15) = happyShift action_11
action_22 (18) = happyShift action_12
action_22 (20) = happyShift action_13
action_22 (23) = happyShift action_14
action_22 (24) = happyShift action_15
action_22 (27) = happyShift action_16
action_22 (7) = happyGoto action_43
action_22 (8) = happyGoto action_5
action_22 (10) = happyGoto action_6
action_22 (11) = happyGoto action_7
action_22 (12) = happyGoto action_8
action_22 _ = happyFail

action_23 _ = happyReduce_8

action_24 (19) = happyShift action_42
action_24 _ = happyFail

action_25 (19) = happyShift action_41
action_25 (25) = happyShift action_30
action_25 (29) = happyShift action_31
action_25 _ = happyFail

action_26 (13) = happyShift action_9
action_26 (18) = happyShift action_35
action_26 (27) = happyShift action_16
action_26 (9) = happyGoto action_40
action_26 (10) = happyGoto action_39
action_26 (11) = happyGoto action_7
action_26 (12) = happyGoto action_8
action_26 _ = happyFail

action_27 (13) = happyShift action_9
action_27 (18) = happyShift action_35
action_27 (27) = happyShift action_16
action_27 (9) = happyGoto action_38
action_27 (10) = happyGoto action_39
action_27 (11) = happyGoto action_7
action_27 (12) = happyGoto action_8
action_27 _ = happyFail

action_28 _ = happyReduce_25

action_29 (13) = happyShift action_9
action_29 (18) = happyShift action_35
action_29 (27) = happyShift action_16
action_29 (12) = happyGoto action_37
action_29 _ = happyFail

action_30 (13) = happyShift action_9
action_30 (18) = happyShift action_35
action_30 (27) = happyShift action_16
action_30 (11) = happyGoto action_36
action_30 (12) = happyGoto action_8
action_30 _ = happyFail

action_31 (13) = happyShift action_9
action_31 (18) = happyShift action_35
action_31 (27) = happyShift action_16
action_31 (10) = happyGoto action_34
action_31 (11) = happyGoto action_7
action_31 (12) = happyGoto action_8
action_31 _ = happyFail

action_32 (13) = happyShift action_9
action_32 (14) = happyShift action_10
action_32 (15) = happyShift action_11
action_32 (18) = happyShift action_12
action_32 (20) = happyShift action_13
action_32 (23) = happyShift action_14
action_32 (24) = happyShift action_15
action_32 (27) = happyShift action_16
action_32 (7) = happyGoto action_33
action_32 (8) = happyGoto action_5
action_32 (10) = happyGoto action_6
action_32 (11) = happyGoto action_7
action_32 (12) = happyGoto action_8
action_32 _ = happyFail

action_33 _ = happyReduce_6

action_34 (25) = happyShift action_30
action_34 _ = happyReduce_14

action_35 (13) = happyShift action_9
action_35 (18) = happyShift action_35
action_35 (27) = happyShift action_16
action_35 (10) = happyGoto action_50
action_35 (11) = happyGoto action_7
action_35 (12) = happyGoto action_8
action_35 _ = happyFail

action_36 (26) = happyShift action_29
action_36 _ = happyReduce_19

action_37 (28) = happyShift action_28
action_37 _ = happyReduce_21

action_38 (19) = happyShift action_49
action_38 _ = happyFail

action_39 (22) = happyShift action_48
action_39 (25) = happyShift action_30
action_39 _ = happyReduce_16

action_40 (19) = happyShift action_47
action_40 _ = happyFail

action_41 _ = happyReduce_23

action_42 _ = happyReduce_9

action_43 _ = happyReduce_10

action_44 _ = happyReduce_11

action_45 _ = happyReduce_2

action_46 (16) = happyShift action_32
action_46 _ = happyReduce_4

action_47 _ = happyReduce_13

action_48 (13) = happyShift action_9
action_48 (18) = happyShift action_35
action_48 (27) = happyShift action_16
action_48 (9) = happyGoto action_51
action_48 (10) = happyGoto action_39
action_48 (11) = happyGoto action_7
action_48 (12) = happyGoto action_8
action_48 _ = happyFail

action_49 _ = happyReduce_26

action_50 (19) = happyShift action_41
action_50 (25) = happyShift action_30
action_50 _ = happyFail

action_51 _ = happyReduce_17

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (Impl happy_var_1 happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  5 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Or happy_var_1 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  6 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (And happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  7 happyReduction_7
happyReduction_7 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  7 happyReduction_8
happyReduction_8 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (Not happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  7 happyReduction_9
happyReduction_9 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  7 happyReduction_10
happyReduction_10 (HappyAbsSyn7  happy_var_3)
	(HappyTerminal (TVar happy_var_2))
	_
	 =  HappyAbsSyn7
		 (Each happy_var_2 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  7 happyReduction_11
happyReduction_11 (HappyAbsSyn7  happy_var_3)
	(HappyTerminal (TVar happy_var_2))
	_
	 =  HappyAbsSyn7
		 (Exist happy_var_2 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  7 happyReduction_12
happyReduction_12 (HappyTerminal (TTmp happy_var_1))
	 =  HappyAbsSyn7
		 (Template happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happyReduce 4 8 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TPr happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (SomePredicate happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_3  8 happyReduction_14
happyReduction_14 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn8
		 (EqualPredicate happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  8 happyReduction_15
happyReduction_15 (HappyTerminal (TPr happy_var_1))
	 =  HappyAbsSyn8
		 (SomePredicate happy_var_1 []
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  9 happyReduction_16
happyReduction_16 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  9 happyReduction_17
happyReduction_17 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 : happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  10 happyReduction_18
happyReduction_18 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  10 happyReduction_19
happyReduction_19 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Sum happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  11 happyReduction_20
happyReduction_20 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  11 happyReduction_21
happyReduction_21 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (Mul happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  12 happyReduction_22
happyReduction_22 (HappyTerminal (TVar happy_var_1))
	 =  HappyAbsSyn12
		 (Var happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  12 happyReduction_23
happyReduction_23 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  12 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn12
		 (Zero
	)

happyReduce_25 = happySpecReduce_2  12 happyReduction_25
happyReduction_25 _
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Nxt happy_var_1
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happyReduce 4 12 happyReduction_26
happyReduction_26 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (Fun happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 30 30 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TVar happy_dollar_dollar -> cont 13;
	TPr happy_dollar_dollar -> cont 14;
	TTmp happy_dollar_dollar -> cont 15;
	TAnd -> cont 16;
	TOr -> cont 17;
	TOB -> cont 18;
	TCB -> cont 19;
	TNot -> cont 20;
	TImpl -> cont 21;
	TComma -> cont 22;
	TAll -> cont 23;
	TEx -> cont 24;
	TPlus -> cont 25;
	TMul -> cont 26;
	TZero -> cont 27;
	TNxt -> cont 28;
	TEq -> cont 29;
	_ -> happyError' (tk:tks)
	}

happyError_ 30 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parser tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError (xs) = error ("Parse error on token " ++ (show xs))
parseError [] = error ("Parse error on token at the end")

data Token = 
				TVar String
      | TAnd
      | TOr
      | TNot
      | TImpl
      | TOB
      | TCB
      | TEol
      | TComma
      | TTrq
      | TAll
      | TEx
      | TPlus
      | TMul
      | TZero
      | TNxt
      | TEq
      | TPr String
      | TTmp String
      | TOSB
      | TCSB
      | TAs
 			deriving Show

isLowerAlphaOrNumber c = ((isAlpha c) && (isLower c)) || (isNumber c)
isUpperAlphaOrNumber c = ((isAlpha c) && (isUpper c)) || (isNumber c)
isAlphaOrNumber c = ((isAlpha c)) || (isNumber c)


lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | isSpace c = lexer cs
      | ((isUpper c) && isAlpha c) = lexPred (c:cs)
      | ((isLower c) && isAlpha c) = lexAlpha (c:cs)
lexer ('&':cs) = TAnd : lexer cs
lexer ('|':'-':cs) = TTrq : lexer cs
lexer ('|':cs) = TOr : lexer cs
lexer ('-':'>':cs) = TImpl : lexer cs
lexer ('!':cs) = TNot : lexer cs
lexer ('(':cs) = TOB : lexer cs
lexer (')':cs) = TCB : lexer cs
lexer ('\n':cs) = TEol : lexer cs
lexer (',':cs) = TComma : lexer cs
lexer ('@':cs) = TAll : lexer cs
lexer ('?':cs) = TEx : lexer cs
lexer ('+':cs) = TPlus : lexer cs
lexer ('*':cs) = TMul : lexer cs
lexer ('0':cs) = TZero : lexer cs
lexer ('\'':cs) = TNxt : lexer cs
lexer ('=':cs) = TEq : lexer cs
lexer ('#':cs) = (lexTmp cs)
lexer ('0':cs) = TZero : lexer cs


lexAlpha cs = TVar var : lexer rest
      where (var,rest) = span isLowerAlphaOrNumber cs

lexTmp cs = TTmp var : lexer rest
      where (var,rest) = span isAlphaOrNumber cs

lexPred cs = TPr var : lexer rest
      where (var,rest) = span isUpperAlphaOrNumber cs
      
parse = parser . lexer
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<command-line>" #-}





# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 5 "<command-line>" 2
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 45 "templates/GenericTemplate.hs" #-}








{-# LINE 66 "templates/GenericTemplate.hs" #-}

{-# LINE 76 "templates/GenericTemplate.hs" #-}

{-# LINE 85 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 154 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 255 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 321 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
