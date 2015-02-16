{-# OPTIONS_GHC -w #-}
module FileParser where
import Data.Char
import Prelude
import Expression

-- parser produced by Happy Version 1.19.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15
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
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15

action_0 (16) = happyShift action_11
action_0 (17) = happyShift action_12
action_0 (20) = happyShift action_13
action_0 (22) = happyShift action_14
action_0 (25) = happyShift action_15
action_0 (26) = happyShift action_16
action_0 (29) = happyShift action_17
action_0 (4) = happyGoto action_18
action_0 (5) = happyGoto action_19
action_0 (7) = happyGoto action_3
action_0 (8) = happyGoto action_4
action_0 (9) = happyGoto action_5
action_0 (10) = happyGoto action_6
action_0 (11) = happyGoto action_7
action_0 (13) = happyGoto action_8
action_0 (14) = happyGoto action_9
action_0 (15) = happyGoto action_10
action_0 _ = happyFail

action_1 (16) = happyShift action_11
action_1 (17) = happyShift action_12
action_1 (20) = happyShift action_13
action_1 (22) = happyShift action_14
action_1 (25) = happyShift action_15
action_1 (26) = happyShift action_16
action_1 (29) = happyShift action_17
action_1 (5) = happyGoto action_2
action_1 (7) = happyGoto action_3
action_1 (8) = happyGoto action_4
action_1 (9) = happyGoto action_5
action_1 (10) = happyGoto action_6
action_1 (11) = happyGoto action_7
action_1 (13) = happyGoto action_8
action_1 (14) = happyGoto action_9
action_1 (15) = happyGoto action_10
action_1 _ = happyFail

action_2 (32) = happyShift action_20
action_2 _ = happyFail

action_3 (24) = happyShift action_35
action_3 (33) = happyShift action_36
action_3 _ = happyFail

action_4 (19) = happyShift action_33
action_4 (23) = happyShift action_34
action_4 _ = happyReduce_7

action_5 (18) = happyShift action_32
action_5 _ = happyReduce_9

action_6 _ = happyReduce_11

action_7 _ = happyReduce_13

action_8 (27) = happyShift action_30
action_8 (31) = happyShift action_31
action_8 _ = happyFail

action_9 (28) = happyShift action_29
action_9 _ = happyReduce_22

action_10 (30) = happyShift action_28
action_10 _ = happyReduce_24

action_11 (20) = happyShift action_27
action_11 _ = happyReduce_26

action_12 (20) = happyShift action_26
action_12 _ = happyFail

action_13 (16) = happyShift action_11
action_13 (17) = happyShift action_12
action_13 (20) = happyShift action_13
action_13 (22) = happyShift action_14
action_13 (25) = happyShift action_15
action_13 (26) = happyShift action_16
action_13 (29) = happyShift action_17
action_13 (7) = happyGoto action_24
action_13 (8) = happyGoto action_4
action_13 (9) = happyGoto action_5
action_13 (10) = happyGoto action_6
action_13 (11) = happyGoto action_7
action_13 (13) = happyGoto action_25
action_13 (14) = happyGoto action_9
action_13 (15) = happyGoto action_10
action_13 _ = happyFail

action_14 (16) = happyShift action_11
action_14 (17) = happyShift action_12
action_14 (20) = happyShift action_13
action_14 (22) = happyShift action_14
action_14 (25) = happyShift action_15
action_14 (26) = happyShift action_16
action_14 (29) = happyShift action_17
action_14 (10) = happyGoto action_23
action_14 (11) = happyGoto action_7
action_14 (13) = happyGoto action_8
action_14 (14) = happyGoto action_9
action_14 (15) = happyGoto action_10
action_14 _ = happyFail

action_15 (16) = happyShift action_22
action_15 _ = happyFail

action_16 (16) = happyShift action_21
action_16 _ = happyFail

action_17 _ = happyReduce_28

action_18 (34) = happyAccept
action_18 _ = happyFail

action_19 (32) = happyShift action_20
action_19 _ = happyReduce_2

action_20 (16) = happyShift action_11
action_20 (17) = happyShift action_12
action_20 (20) = happyShift action_13
action_20 (22) = happyShift action_14
action_20 (25) = happyShift action_15
action_20 (26) = happyShift action_16
action_20 (29) = happyShift action_17
action_20 (6) = happyGoto action_53
action_20 (7) = happyGoto action_54
action_20 (8) = happyGoto action_4
action_20 (9) = happyGoto action_5
action_20 (10) = happyGoto action_6
action_20 (11) = happyGoto action_7
action_20 (13) = happyGoto action_8
action_20 (14) = happyGoto action_9
action_20 (15) = happyGoto action_10
action_20 _ = happyFail

action_21 (16) = happyShift action_11
action_21 (17) = happyShift action_12
action_21 (20) = happyShift action_13
action_21 (22) = happyShift action_14
action_21 (25) = happyShift action_15
action_21 (26) = happyShift action_16
action_21 (29) = happyShift action_17
action_21 (10) = happyGoto action_52
action_21 (11) = happyGoto action_7
action_21 (13) = happyGoto action_8
action_21 (14) = happyGoto action_9
action_21 (15) = happyGoto action_10
action_21 _ = happyFail

action_22 (16) = happyShift action_11
action_22 (17) = happyShift action_12
action_22 (20) = happyShift action_13
action_22 (22) = happyShift action_14
action_22 (25) = happyShift action_15
action_22 (26) = happyShift action_16
action_22 (29) = happyShift action_17
action_22 (10) = happyGoto action_51
action_22 (11) = happyGoto action_7
action_22 (13) = happyGoto action_8
action_22 (14) = happyGoto action_9
action_22 (15) = happyGoto action_10
action_22 _ = happyFail

action_23 _ = happyReduce_14

action_24 (21) = happyShift action_50
action_24 _ = happyFail

action_25 (21) = happyShift action_49
action_25 (27) = happyShift action_30
action_25 (31) = happyShift action_31
action_25 _ = happyFail

action_26 (16) = happyShift action_11
action_26 (20) = happyShift action_43
action_26 (29) = happyShift action_17
action_26 (12) = happyGoto action_48
action_26 (13) = happyGoto action_47
action_26 (14) = happyGoto action_9
action_26 (15) = happyGoto action_10
action_26 _ = happyFail

action_27 (16) = happyShift action_11
action_27 (20) = happyShift action_43
action_27 (29) = happyShift action_17
action_27 (12) = happyGoto action_46
action_27 (13) = happyGoto action_47
action_27 (14) = happyGoto action_9
action_27 (15) = happyGoto action_10
action_27 _ = happyFail

action_28 _ = happyReduce_29

action_29 (16) = happyShift action_11
action_29 (20) = happyShift action_43
action_29 (29) = happyShift action_17
action_29 (15) = happyGoto action_45
action_29 _ = happyFail

action_30 (16) = happyShift action_11
action_30 (20) = happyShift action_43
action_30 (29) = happyShift action_17
action_30 (14) = happyGoto action_44
action_30 (15) = happyGoto action_10
action_30 _ = happyFail

action_31 (16) = happyShift action_11
action_31 (20) = happyShift action_43
action_31 (29) = happyShift action_17
action_31 (13) = happyGoto action_42
action_31 (14) = happyGoto action_9
action_31 (15) = happyGoto action_10
action_31 _ = happyFail

action_32 (16) = happyShift action_11
action_32 (17) = happyShift action_12
action_32 (20) = happyShift action_13
action_32 (22) = happyShift action_14
action_32 (25) = happyShift action_15
action_32 (26) = happyShift action_16
action_32 (29) = happyShift action_17
action_32 (10) = happyGoto action_41
action_32 (11) = happyGoto action_7
action_32 (13) = happyGoto action_8
action_32 (14) = happyGoto action_9
action_32 (15) = happyGoto action_10
action_32 _ = happyFail

action_33 (16) = happyShift action_11
action_33 (17) = happyShift action_12
action_33 (20) = happyShift action_13
action_33 (22) = happyShift action_14
action_33 (25) = happyShift action_15
action_33 (26) = happyShift action_16
action_33 (29) = happyShift action_17
action_33 (9) = happyGoto action_40
action_33 (10) = happyGoto action_6
action_33 (11) = happyGoto action_7
action_33 (13) = happyGoto action_8
action_33 (14) = happyGoto action_9
action_33 (15) = happyGoto action_10
action_33 _ = happyFail

action_34 (16) = happyShift action_11
action_34 (17) = happyShift action_12
action_34 (20) = happyShift action_13
action_34 (22) = happyShift action_14
action_34 (25) = happyShift action_15
action_34 (26) = happyShift action_16
action_34 (29) = happyShift action_17
action_34 (7) = happyGoto action_39
action_34 (8) = happyGoto action_4
action_34 (9) = happyGoto action_5
action_34 (10) = happyGoto action_6
action_34 (11) = happyGoto action_7
action_34 (13) = happyGoto action_8
action_34 (14) = happyGoto action_9
action_34 (15) = happyGoto action_10
action_34 _ = happyFail

action_35 (16) = happyShift action_11
action_35 (17) = happyShift action_12
action_35 (20) = happyShift action_13
action_35 (22) = happyShift action_14
action_35 (25) = happyShift action_15
action_35 (26) = happyShift action_16
action_35 (29) = happyShift action_17
action_35 (5) = happyGoto action_38
action_35 (7) = happyGoto action_3
action_35 (8) = happyGoto action_4
action_35 (9) = happyGoto action_5
action_35 (10) = happyGoto action_6
action_35 (11) = happyGoto action_7
action_35 (13) = happyGoto action_8
action_35 (14) = happyGoto action_9
action_35 (15) = happyGoto action_10
action_35 _ = happyFail

action_36 (16) = happyShift action_11
action_36 (17) = happyShift action_12
action_36 (20) = happyShift action_13
action_36 (22) = happyShift action_14
action_36 (25) = happyShift action_15
action_36 (26) = happyShift action_16
action_36 (29) = happyShift action_17
action_36 (7) = happyGoto action_37
action_36 (8) = happyGoto action_4
action_36 (9) = happyGoto action_5
action_36 (10) = happyGoto action_6
action_36 (11) = happyGoto action_7
action_36 (13) = happyGoto action_8
action_36 (14) = happyGoto action_9
action_36 (15) = happyGoto action_10
action_36 _ = happyFail

action_37 _ = happyReduce_4

action_38 _ = happyReduce_3

action_39 _ = happyReduce_8

action_40 (18) = happyShift action_32
action_40 _ = happyReduce_10

action_41 _ = happyReduce_12

action_42 (27) = happyShift action_30
action_42 _ = happyReduce_19

action_43 (16) = happyShift action_11
action_43 (20) = happyShift action_43
action_43 (29) = happyShift action_17
action_43 (13) = happyGoto action_59
action_43 (14) = happyGoto action_9
action_43 (15) = happyGoto action_10
action_43 _ = happyFail

action_44 (28) = happyShift action_29
action_44 _ = happyReduce_23

action_45 (30) = happyShift action_28
action_45 _ = happyReduce_25

action_46 (21) = happyShift action_58
action_46 _ = happyFail

action_47 (24) = happyShift action_57
action_47 (27) = happyShift action_30
action_47 _ = happyReduce_20

action_48 (21) = happyShift action_56
action_48 _ = happyFail

action_49 _ = happyReduce_27

action_50 _ = happyReduce_15

action_51 _ = happyReduce_16

action_52 _ = happyReduce_17

action_53 _ = happyReduce_1

action_54 (32) = happyShift action_55
action_54 _ = happyReduce_6

action_55 (16) = happyShift action_11
action_55 (17) = happyShift action_12
action_55 (20) = happyShift action_13
action_55 (22) = happyShift action_14
action_55 (25) = happyShift action_15
action_55 (26) = happyShift action_16
action_55 (29) = happyShift action_17
action_55 (6) = happyGoto action_61
action_55 (7) = happyGoto action_54
action_55 (8) = happyGoto action_4
action_55 (9) = happyGoto action_5
action_55 (10) = happyGoto action_6
action_55 (11) = happyGoto action_7
action_55 (13) = happyGoto action_8
action_55 (14) = happyGoto action_9
action_55 (15) = happyGoto action_10
action_55 _ = happyFail

action_56 _ = happyReduce_18

action_57 (16) = happyShift action_11
action_57 (20) = happyShift action_43
action_57 (29) = happyShift action_17
action_57 (12) = happyGoto action_60
action_57 (13) = happyGoto action_47
action_57 (14) = happyGoto action_9
action_57 (15) = happyGoto action_10
action_57 _ = happyFail

action_58 _ = happyReduce_30

action_59 (21) = happyShift action_49
action_59 (27) = happyShift action_30
action_59 _ = happyFail

action_60 _ = happyReduce_21

action_61 _ = happyReduce_5

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (InFile (assumptions happy_var_1) (statement happy_var_1) happy_var_3
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (InFile (assumptions happy_var_1) (statement happy_var_1) []
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 (appendAssumption happy_var_3 happy_var_1
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  5 happyReduction_4
happyReduction_4 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 (InFile [happy_var_1] happy_var_3 []
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  6 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 : happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  6 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  7 happyReduction_7
happyReduction_7 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  7 happyReduction_8
happyReduction_8 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (Impl happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  8 happyReduction_9
happyReduction_9 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  8 happyReduction_10
happyReduction_10 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Or happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  9 happyReduction_11
happyReduction_11 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  9 happyReduction_12
happyReduction_12 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (And happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  10 happyReduction_13
happyReduction_13 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  10 happyReduction_14
happyReduction_14 (HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (Not happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  10 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  10 happyReduction_16
happyReduction_16 (HappyAbsSyn10  happy_var_3)
	(HappyTerminal (TVar happy_var_2))
	_
	 =  HappyAbsSyn10
		 (Each happy_var_2 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  10 happyReduction_17
happyReduction_17 (HappyAbsSyn10  happy_var_3)
	(HappyTerminal (TVar happy_var_2))
	_
	 =  HappyAbsSyn10
		 (Exist happy_var_2 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 4 11 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TPr happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (SomePr happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_3  11 happyReduction_19
happyReduction_19 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn11
		 (EqPr happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  12 happyReduction_20
happyReduction_20 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  12 happyReduction_21
happyReduction_21 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 : happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  13 happyReduction_22
happyReduction_22 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  13 happyReduction_23
happyReduction_23 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Sum happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  14 happyReduction_24
happyReduction_24 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  14 happyReduction_25
happyReduction_25 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (Mul happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  15 happyReduction_26
happyReduction_26 (HappyTerminal (TVar happy_var_1))
	 =  HappyAbsSyn15
		 (Var happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  15 happyReduction_27
happyReduction_27 _
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (happy_var_2
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  15 happyReduction_28
happyReduction_28 _
	 =  HappyAbsSyn15
		 (Zero
	)

happyReduce_29 = happySpecReduce_2  15 happyReduction_29
happyReduction_29 _
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (Nxt happy_var_1
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happyReduce 4 15 happyReduction_30
happyReduction_30 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Fun happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 34 34 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TVar happy_dollar_dollar -> cont 16;
	TPr happy_dollar_dollar -> cont 17;
	TAnd -> cont 18;
	TOr -> cont 19;
	TOB -> cont 20;
	TCB -> cont 21;
	TNot -> cont 22;
	TImpl -> cont 23;
	TComma -> cont 24;
	TAll -> cont 25;
	TEx -> cont 26;
	TPlus -> cont 27;
	TMul -> cont 28;
	TZero -> cont 29;
	TNxt -> cont 30;
	TEq -> cont 31;
	TEol -> cont 32;
	TTrq -> cont 33;
	_ -> happyError' (tk:tks)
	}

happyError_ 34 tk tks = happyError' tks
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

fileParser tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError (s:xs) = error ("Parse error on token " ++ (show s))
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
 			deriving Show

isLowerAlphaOrNumber c = ((isAlpha c) && (isLower c)) || (isNumber c)
isUpperAlphaOrNumber c = ((isAlpha c) && (isUpper c)) || (isNumber c)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | isSpace c = lexer cs
      | (isUpperAlphaOrNumber c) = lexPred (c:cs)
      | (isLowerAlphaOrNumber c) = lexAlpha (c:cs)
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

lexAlpha cs = TVar var : lexer rest
      where (var,rest) = span isLowerAlphaOrNumber cs

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
