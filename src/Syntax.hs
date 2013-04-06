{-# OPTIONS_GHC -w #-}
module Syntax where

import AST
import Lex
import Tokens

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn 
	= HappyTerminal (TokenType)
	| HappyErrorToken Int
	| HappyAbsSyn4 ([Module])
	| HappyAbsSyn6 (Module)
	| HappyAbsSyn7 ([String])
	| HappyAbsSyn10 (ModuleStms)
	| HappyAbsSyn11 ([PortDecl])
	| HappyAbsSyn12 (PortDecl)
	| HappyAbsSyn13 (Direction)
	| HappyAbsSyn14 (Digit)
	| HappyAbsSyn16 ([Wire])
	| HappyAbsSyn17 (Wire)
	| HappyAbsSyn18 (WireType)
	| HappyAbsSyn19 ([Parameter])
	| HappyAbsSyn20 (Parameter)
	| HappyAbsSyn22 ([Stm])
	| HappyAbsSyn23 (Stm)
	| HappyAbsSyn26 (Range)
	| HappyAbsSyn27 (Expr)
	| HappyAbsSyn28 ([Expr])
	| HappyAbsSyn29 (Const)
	| HappyAbsSyn31 (SubmoduleFlist)
	| HappyAbsSyn33 ([(String,Expr)])
	| HappyAbsSyn34 ((String,Expr))

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (TokenType)
	-> HappyState (TokenType) (HappyStk HappyAbsSyn -> [(TokenType)] -> m HappyAbsSyn)
	-> [HappyState (TokenType) (HappyStk HappyAbsSyn -> [(TokenType)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(TokenType)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128 :: () => Int -> ({-HappyReduction (Either String) = -}
	   Int 
	-> (TokenType)
	-> HappyState (TokenType) (HappyStk HappyAbsSyn -> [(TokenType)] -> (Either String) HappyAbsSyn)
	-> [HappyState (TokenType) (HappyStk HappyAbsSyn -> [(TokenType)] -> (Either String) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(TokenType)] -> (Either String) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73 :: () => ({-HappyReduction (Either String) = -}
	   Int 
	-> (TokenType)
	-> HappyState (TokenType) (HappyStk HappyAbsSyn -> [(TokenType)] -> (Either String) HappyAbsSyn)
	-> [HappyState (TokenType) (HappyStk HappyAbsSyn -> [(TokenType)] -> (Either String) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(TokenType)] -> (Either String) HappyAbsSyn)

action_0 (35) = happyShift action_4
action_0 (67) = happyShift action_5
action_0 (4) = happyGoto action_6
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 _ = happyFail

action_1 (35) = happyShift action_4
action_1 (67) = happyShift action_5
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (35) = happyShift action_4
action_3 (67) = happyShift action_5
action_3 (5) = happyGoto action_8
action_3 (6) = happyGoto action_3
action_3 _ = happyReduce_2

action_4 (65) = happyShift action_7
action_4 _ = happyFail

action_5 _ = happyReduce_4

action_6 (68) = happyAccept
action_6 _ = happyFail

action_7 (45) = happyShift action_9
action_7 _ = happyFail

action_8 _ = happyReduce_3

action_9 (65) = happyShift action_11
action_9 (7) = happyGoto action_10
action_9 _ = happyReduce_8

action_10 (46) = happyShift action_13
action_10 _ = happyFail

action_11 (54) = happyShift action_12
action_11 _ = happyReduce_6

action_12 (65) = happyShift action_11
action_12 (7) = happyGoto action_15
action_12 _ = happyReduce_8

action_13 (52) = happyShift action_14
action_13 _ = happyFail

action_14 (40) = happyShift action_20
action_14 (41) = happyShift action_21
action_14 (42) = happyShift action_22
action_14 (10) = happyGoto action_16
action_14 (11) = happyGoto action_17
action_14 (12) = happyGoto action_18
action_14 (13) = happyGoto action_19
action_14 _ = happyReduce_15

action_15 _ = happyReduce_7

action_16 (36) = happyShift action_31
action_16 _ = happyFail

action_17 (43) = happyShift action_29
action_17 (44) = happyShift action_30
action_17 (16) = happyGoto action_26
action_17 (17) = happyGoto action_27
action_17 (18) = happyGoto action_28
action_17 _ = happyReduce_27

action_18 (52) = happyShift action_25
action_18 _ = happyFail

action_19 (49) = happyShift action_24
action_19 (26) = happyGoto action_23
action_19 _ = happyReduce_47

action_20 _ = happyReduce_17

action_21 _ = happyReduce_18

action_22 _ = happyReduce_19

action_23 (65) = happyShift action_53
action_23 (8) = happyGoto action_52
action_23 _ = happyFail

action_24 (47) = happyShift action_43
action_24 (57) = happyShift action_44
action_24 (60) = happyShift action_45
action_24 (61) = happyShift action_46
action_24 (62) = happyShift action_47
action_24 (63) = happyShift action_48
action_24 (64) = happyShift action_49
action_24 (65) = happyShift action_50
action_24 (66) = happyShift action_51
action_24 (14) = happyGoto action_38
action_24 (15) = happyGoto action_39
action_24 (21) = happyGoto action_40
action_24 (27) = happyGoto action_41
action_24 (29) = happyGoto action_42
action_24 _ = happyFail

action_25 (36) = happyReduce_15
action_25 (37) = happyReduce_15
action_25 (38) = happyReduce_15
action_25 (39) = happyReduce_15
action_25 (40) = happyShift action_20
action_25 (41) = happyShift action_21
action_25 (42) = happyShift action_22
action_25 (43) = happyReduce_15
action_25 (44) = happyReduce_15
action_25 (65) = happyReduce_15
action_25 (11) = happyGoto action_37
action_25 (12) = happyGoto action_18
action_25 (13) = happyGoto action_19
action_25 _ = happyReduce_15

action_26 (38) = happyShift action_36
action_26 (19) = happyGoto action_34
action_26 (20) = happyGoto action_35
action_26 _ = happyReduce_33

action_27 (52) = happyShift action_33
action_27 _ = happyFail

action_28 (49) = happyShift action_24
action_28 (26) = happyGoto action_32
action_28 _ = happyReduce_47

action_29 _ = happyReduce_29

action_30 _ = happyReduce_30

action_31 _ = happyReduce_5

action_32 (65) = happyShift action_53
action_32 (8) = happyGoto action_81
action_32 _ = happyFail

action_33 (36) = happyReduce_27
action_33 (37) = happyReduce_27
action_33 (38) = happyReduce_27
action_33 (39) = happyReduce_27
action_33 (43) = happyShift action_29
action_33 (44) = happyShift action_30
action_33 (65) = happyReduce_27
action_33 (16) = happyGoto action_80
action_33 (17) = happyGoto action_27
action_33 (18) = happyGoto action_28
action_33 _ = happyReduce_27

action_34 (37) = happyShift action_77
action_34 (39) = happyShift action_78
action_34 (65) = happyShift action_79
action_34 (22) = happyGoto action_72
action_34 (23) = happyGoto action_73
action_34 (24) = happyGoto action_74
action_34 (25) = happyGoto action_75
action_34 (30) = happyGoto action_76
action_34 _ = happyReduce_39

action_35 (52) = happyShift action_71
action_35 _ = happyFail

action_36 (65) = happyShift action_70
action_36 _ = happyFail

action_37 _ = happyReduce_14

action_38 _ = happyReduce_35

action_39 _ = happyReduce_36

action_40 _ = happyReduce_59

action_41 (50) = happyShift action_65
action_41 (51) = happyShift action_66
action_41 (56) = happyShift action_67
action_41 (58) = happyShift action_68
action_41 (59) = happyShift action_69
action_41 _ = happyFail

action_42 (46) = happyReduce_49
action_42 (48) = happyReduce_49
action_42 (49) = happyShift action_24
action_42 (50) = happyReduce_49
action_42 (51) = happyReduce_49
action_42 (52) = happyReduce_49
action_42 (53) = happyReduce_49
action_42 (54) = happyReduce_49
action_42 (56) = happyShift action_62
action_42 (58) = happyShift action_63
action_42 (59) = happyShift action_64
action_42 (26) = happyGoto action_61
action_42 _ = happyReduce_49

action_43 (47) = happyShift action_43
action_43 (57) = happyShift action_44
action_43 (60) = happyShift action_45
action_43 (61) = happyShift action_46
action_43 (62) = happyShift action_47
action_43 (63) = happyShift action_48
action_43 (64) = happyShift action_49
action_43 (65) = happyShift action_50
action_43 (66) = happyShift action_51
action_43 (14) = happyGoto action_38
action_43 (15) = happyGoto action_39
action_43 (21) = happyGoto action_40
action_43 (27) = happyGoto action_59
action_43 (28) = happyGoto action_60
action_43 (29) = happyGoto action_42
action_43 _ = happyReduce_58

action_44 (47) = happyShift action_43
action_44 (57) = happyShift action_44
action_44 (60) = happyShift action_45
action_44 (61) = happyShift action_46
action_44 (62) = happyShift action_47
action_44 (63) = happyShift action_48
action_44 (64) = happyShift action_49
action_44 (65) = happyShift action_50
action_44 (66) = happyShift action_51
action_44 (14) = happyGoto action_38
action_44 (15) = happyGoto action_39
action_44 (21) = happyGoto action_40
action_44 (27) = happyGoto action_57
action_44 (29) = happyGoto action_58
action_44 _ = happyFail

action_45 _ = happyReduce_23

action_46 _ = happyReduce_24

action_47 _ = happyReduce_20

action_48 _ = happyReduce_21

action_49 _ = happyReduce_22

action_50 (49) = happyShift action_24
action_50 (26) = happyGoto action_56
action_50 _ = happyReduce_47

action_51 _ = happyReduce_60

action_52 _ = happyReduce_16

action_53 (54) = happyShift action_55
action_53 (9) = happyGoto action_54
action_53 _ = happyReduce_11

action_54 _ = happyReduce_9

action_55 (65) = happyShift action_53
action_55 (8) = happyGoto action_100
action_55 _ = happyFail

action_56 _ = happyReduce_48

action_57 (56) = happyShift action_67
action_57 (58) = happyShift action_68
action_57 (59) = happyShift action_69
action_57 _ = happyReduce_53

action_58 (46) = happyReduce_61
action_58 (48) = happyReduce_61
action_58 (49) = happyShift action_24
action_58 (50) = happyReduce_61
action_58 (51) = happyReduce_61
action_58 (52) = happyReduce_61
action_58 (53) = happyReduce_61
action_58 (54) = happyReduce_61
action_58 (56) = happyShift action_62
action_58 (58) = happyShift action_63
action_58 (59) = happyShift action_64
action_58 (26) = happyGoto action_61
action_58 _ = happyReduce_61

action_59 (54) = happyShift action_99
action_59 (56) = happyShift action_67
action_59 (58) = happyShift action_68
action_59 (59) = happyShift action_69
action_59 _ = happyReduce_56

action_60 (48) = happyShift action_98
action_60 _ = happyFail

action_61 _ = happyReduce_50

action_62 (57) = happyShift action_95
action_62 (60) = happyShift action_45
action_62 (61) = happyShift action_46
action_62 (62) = happyShift action_47
action_62 (63) = happyShift action_48
action_62 (64) = happyShift action_49
action_62 (66) = happyShift action_51
action_62 (14) = happyGoto action_38
action_62 (15) = happyGoto action_39
action_62 (21) = happyGoto action_40
action_62 (29) = happyGoto action_97
action_62 _ = happyFail

action_63 (57) = happyShift action_95
action_63 (60) = happyShift action_45
action_63 (61) = happyShift action_46
action_63 (62) = happyShift action_47
action_63 (63) = happyShift action_48
action_63 (64) = happyShift action_49
action_63 (66) = happyShift action_51
action_63 (14) = happyGoto action_38
action_63 (15) = happyGoto action_39
action_63 (21) = happyGoto action_40
action_63 (29) = happyGoto action_96
action_63 _ = happyFail

action_64 (57) = happyShift action_95
action_64 (60) = happyShift action_45
action_64 (61) = happyShift action_46
action_64 (62) = happyShift action_47
action_64 (63) = happyShift action_48
action_64 (64) = happyShift action_49
action_64 (66) = happyShift action_51
action_64 (14) = happyGoto action_38
action_64 (15) = happyGoto action_39
action_64 (21) = happyGoto action_40
action_64 (29) = happyGoto action_94
action_64 _ = happyFail

action_65 _ = happyReduce_46

action_66 (47) = happyShift action_43
action_66 (57) = happyShift action_44
action_66 (60) = happyShift action_45
action_66 (61) = happyShift action_46
action_66 (62) = happyShift action_47
action_66 (63) = happyShift action_48
action_66 (64) = happyShift action_49
action_66 (65) = happyShift action_50
action_66 (66) = happyShift action_51
action_66 (14) = happyGoto action_38
action_66 (15) = happyGoto action_39
action_66 (21) = happyGoto action_40
action_66 (27) = happyGoto action_93
action_66 (29) = happyGoto action_42
action_66 _ = happyFail

action_67 (47) = happyShift action_43
action_67 (57) = happyShift action_44
action_67 (60) = happyShift action_45
action_67 (61) = happyShift action_46
action_67 (62) = happyShift action_47
action_67 (63) = happyShift action_48
action_67 (64) = happyShift action_49
action_67 (65) = happyShift action_50
action_67 (66) = happyShift action_51
action_67 (14) = happyGoto action_38
action_67 (15) = happyGoto action_39
action_67 (21) = happyGoto action_40
action_67 (27) = happyGoto action_92
action_67 (29) = happyGoto action_42
action_67 _ = happyFail

action_68 (47) = happyShift action_43
action_68 (57) = happyShift action_44
action_68 (60) = happyShift action_45
action_68 (61) = happyShift action_46
action_68 (62) = happyShift action_47
action_68 (63) = happyShift action_48
action_68 (64) = happyShift action_49
action_68 (65) = happyShift action_50
action_68 (66) = happyShift action_51
action_68 (14) = happyGoto action_38
action_68 (15) = happyGoto action_39
action_68 (21) = happyGoto action_40
action_68 (27) = happyGoto action_91
action_68 (29) = happyGoto action_42
action_68 _ = happyFail

action_69 (47) = happyShift action_43
action_69 (57) = happyShift action_44
action_69 (60) = happyShift action_45
action_69 (61) = happyShift action_46
action_69 (62) = happyShift action_47
action_69 (63) = happyShift action_48
action_69 (64) = happyShift action_49
action_69 (65) = happyShift action_50
action_69 (66) = happyShift action_51
action_69 (14) = happyGoto action_38
action_69 (15) = happyGoto action_39
action_69 (21) = happyGoto action_40
action_69 (27) = happyGoto action_90
action_69 (29) = happyGoto action_42
action_69 _ = happyFail

action_70 (53) = happyShift action_89
action_70 _ = happyFail

action_71 (36) = happyReduce_33
action_71 (37) = happyReduce_33
action_71 (38) = happyShift action_36
action_71 (39) = happyReduce_33
action_71 (65) = happyReduce_33
action_71 (19) = happyGoto action_88
action_71 (20) = happyGoto action_35
action_71 _ = happyReduce_33

action_72 _ = happyReduce_12

action_73 (52) = happyShift action_87
action_73 _ = happyFail

action_74 _ = happyReduce_42

action_75 _ = happyReduce_40

action_76 _ = happyReduce_41

action_77 (47) = happyShift action_43
action_77 (57) = happyShift action_44
action_77 (60) = happyShift action_45
action_77 (61) = happyShift action_46
action_77 (62) = happyShift action_47
action_77 (63) = happyShift action_48
action_77 (64) = happyShift action_49
action_77 (65) = happyShift action_50
action_77 (66) = happyShift action_51
action_77 (14) = happyGoto action_38
action_77 (15) = happyGoto action_39
action_77 (21) = happyGoto action_40
action_77 (27) = happyGoto action_86
action_77 (29) = happyGoto action_42
action_77 _ = happyFail

action_78 (65) = happyShift action_85
action_78 _ = happyFail

action_79 (45) = happyShift action_83
action_79 (65) = happyShift action_84
action_79 (31) = happyGoto action_82
action_79 _ = happyFail

action_80 _ = happyReduce_26

action_81 _ = happyReduce_28

action_82 _ = happyReduce_66

action_83 (46) = happyReduce_72
action_83 (47) = happyShift action_43
action_83 (55) = happyShift action_115
action_83 (57) = happyShift action_44
action_83 (60) = happyShift action_45
action_83 (61) = happyShift action_46
action_83 (62) = happyShift action_47
action_83 (63) = happyShift action_48
action_83 (64) = happyShift action_49
action_83 (65) = happyShift action_50
action_83 (66) = happyShift action_51
action_83 (14) = happyGoto action_38
action_83 (15) = happyGoto action_39
action_83 (21) = happyGoto action_40
action_83 (27) = happyGoto action_59
action_83 (28) = happyGoto action_111
action_83 (29) = happyGoto action_42
action_83 (32) = happyGoto action_112
action_83 (33) = happyGoto action_113
action_83 (34) = happyGoto action_114
action_83 _ = happyReduce_72

action_84 (45) = happyShift action_83
action_84 (31) = happyGoto action_110
action_84 _ = happyFail

action_85 (55) = happyShift action_109
action_85 _ = happyFail

action_86 (53) = happyShift action_108
action_86 (56) = happyShift action_67
action_86 (58) = happyShift action_68
action_86 (59) = happyShift action_69
action_86 _ = happyFail

action_87 (36) = happyReduce_39
action_87 (37) = happyShift action_77
action_87 (39) = happyShift action_78
action_87 (65) = happyShift action_79
action_87 (22) = happyGoto action_107
action_87 (23) = happyGoto action_73
action_87 (24) = happyGoto action_74
action_87 (25) = happyGoto action_75
action_87 (30) = happyGoto action_76
action_87 _ = happyReduce_39

action_88 _ = happyReduce_32

action_89 (47) = happyShift action_43
action_89 (57) = happyShift action_44
action_89 (60) = happyShift action_45
action_89 (61) = happyShift action_46
action_89 (62) = happyShift action_47
action_89 (63) = happyShift action_48
action_89 (64) = happyShift action_49
action_89 (65) = happyShift action_50
action_89 (66) = happyShift action_51
action_89 (14) = happyGoto action_38
action_89 (15) = happyGoto action_39
action_89 (21) = happyGoto action_40
action_89 (27) = happyGoto action_106
action_89 (29) = happyGoto action_42
action_89 _ = happyFail

action_90 (56) = happyShift action_67
action_90 _ = happyReduce_55

action_91 (56) = happyShift action_67
action_91 (59) = happyShift action_69
action_91 _ = happyReduce_54

action_92 (51) = happyShift action_105
action_92 (56) = happyShift action_67
action_92 (58) = happyShift action_68
action_92 (59) = happyShift action_69
action_92 _ = happyFail

action_93 (50) = happyShift action_104
action_93 (56) = happyShift action_67
action_93 (58) = happyShift action_68
action_93 (59) = happyShift action_69
action_93 _ = happyFail

action_94 (56) = happyShift action_62
action_94 _ = happyReduce_63

action_95 (57) = happyShift action_95
action_95 (60) = happyShift action_45
action_95 (61) = happyShift action_46
action_95 (62) = happyShift action_47
action_95 (63) = happyShift action_48
action_95 (64) = happyShift action_49
action_95 (66) = happyShift action_51
action_95 (14) = happyGoto action_38
action_95 (15) = happyGoto action_39
action_95 (21) = happyGoto action_40
action_95 (29) = happyGoto action_103
action_95 _ = happyFail

action_96 (56) = happyShift action_62
action_96 (59) = happyShift action_64
action_96 _ = happyReduce_62

action_97 (51) = happyShift action_102
action_97 (56) = happyShift action_62
action_97 (58) = happyShift action_63
action_97 (59) = happyShift action_64
action_97 _ = happyFail

action_98 _ = happyReduce_52

action_99 (47) = happyShift action_43
action_99 (57) = happyShift action_44
action_99 (60) = happyShift action_45
action_99 (61) = happyShift action_46
action_99 (62) = happyShift action_47
action_99 (63) = happyShift action_48
action_99 (64) = happyShift action_49
action_99 (65) = happyShift action_50
action_99 (66) = happyShift action_51
action_99 (14) = happyGoto action_38
action_99 (15) = happyGoto action_39
action_99 (21) = happyGoto action_40
action_99 (27) = happyGoto action_59
action_99 (28) = happyGoto action_101
action_99 (29) = happyGoto action_42
action_99 _ = happyReduce_58

action_100 _ = happyReduce_10

action_101 _ = happyReduce_57

action_102 (57) = happyShift action_95
action_102 (60) = happyShift action_45
action_102 (61) = happyShift action_46
action_102 (62) = happyShift action_47
action_102 (63) = happyShift action_48
action_102 (64) = happyShift action_49
action_102 (66) = happyShift action_51
action_102 (14) = happyGoto action_38
action_102 (15) = happyGoto action_39
action_102 (21) = happyGoto action_40
action_102 (29) = happyGoto action_122
action_102 _ = happyFail

action_103 (56) = happyShift action_62
action_103 (58) = happyShift action_63
action_103 (59) = happyShift action_64
action_103 _ = happyReduce_61

action_104 _ = happyReduce_45

action_105 (47) = happyShift action_43
action_105 (57) = happyShift action_44
action_105 (60) = happyShift action_45
action_105 (61) = happyShift action_46
action_105 (62) = happyShift action_47
action_105 (63) = happyShift action_48
action_105 (64) = happyShift action_49
action_105 (65) = happyShift action_50
action_105 (66) = happyShift action_51
action_105 (14) = happyGoto action_38
action_105 (15) = happyGoto action_39
action_105 (21) = happyGoto action_40
action_105 (27) = happyGoto action_121
action_105 (29) = happyGoto action_42
action_105 _ = happyFail

action_106 (56) = happyShift action_67
action_106 (58) = happyShift action_68
action_106 (59) = happyShift action_69
action_106 _ = happyReduce_34

action_107 _ = happyReduce_38

action_108 (47) = happyShift action_43
action_108 (57) = happyShift action_44
action_108 (60) = happyShift action_45
action_108 (61) = happyShift action_46
action_108 (62) = happyShift action_47
action_108 (63) = happyShift action_48
action_108 (64) = happyShift action_49
action_108 (65) = happyShift action_50
action_108 (66) = happyShift action_51
action_108 (14) = happyGoto action_38
action_108 (15) = happyGoto action_39
action_108 (21) = happyGoto action_40
action_108 (27) = happyGoto action_120
action_108 (29) = happyGoto action_42
action_108 _ = happyFail

action_109 (65) = happyShift action_119
action_109 _ = happyFail

action_110 _ = happyReduce_65

action_111 _ = happyReduce_68

action_112 (46) = happyShift action_118
action_112 _ = happyFail

action_113 _ = happyReduce_69

action_114 (54) = happyShift action_117
action_114 _ = happyReduce_70

action_115 (65) = happyShift action_116
action_115 _ = happyFail

action_116 (45) = happyShift action_125
action_116 _ = happyFail

action_117 (55) = happyShift action_115
action_117 (33) = happyGoto action_124
action_117 (34) = happyGoto action_114
action_117 _ = happyReduce_72

action_118 _ = happyReduce_67

action_119 (53) = happyShift action_123
action_119 _ = happyFail

action_120 (56) = happyShift action_67
action_120 (58) = happyShift action_68
action_120 (59) = happyShift action_69
action_120 _ = happyReduce_44

action_121 _ = happyReduce_51

action_122 _ = happyReduce_64

action_123 (47) = happyShift action_43
action_123 (57) = happyShift action_44
action_123 (60) = happyShift action_45
action_123 (61) = happyShift action_46
action_123 (62) = happyShift action_47
action_123 (63) = happyShift action_48
action_123 (64) = happyShift action_49
action_123 (65) = happyShift action_50
action_123 (66) = happyShift action_51
action_123 (14) = happyGoto action_38
action_123 (15) = happyGoto action_39
action_123 (21) = happyGoto action_40
action_123 (27) = happyGoto action_127
action_123 (29) = happyGoto action_42
action_123 _ = happyFail

action_124 _ = happyReduce_71

action_125 (47) = happyShift action_43
action_125 (57) = happyShift action_44
action_125 (60) = happyShift action_45
action_125 (61) = happyShift action_46
action_125 (62) = happyShift action_47
action_125 (63) = happyShift action_48
action_125 (64) = happyShift action_49
action_125 (65) = happyShift action_50
action_125 (66) = happyShift action_51
action_125 (14) = happyGoto action_38
action_125 (15) = happyGoto action_39
action_125 (21) = happyGoto action_40
action_125 (27) = happyGoto action_126
action_125 (29) = happyGoto action_42
action_125 _ = happyFail

action_126 (46) = happyShift action_128
action_126 (56) = happyShift action_67
action_126 (58) = happyShift action_68
action_126 (59) = happyShift action_69
action_126 _ = happyFail

action_127 (56) = happyShift action_67
action_127 (58) = happyShift action_68
action_127 (59) = happyShift action_69
action_127 _ = happyReduce_43

action_128 _ = happyReduce_73

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1]
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 : happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  5 happyReduction_4
happyReduction_4 _
	 =  HappyAbsSyn4
		 ([]
	)

happyReduce_5 = happyReduce 8 6 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Module happy_var_2 happy_var_4 happy_var_7
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 (HappyTerminal (TId happy_var_1))
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  7 happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_3)
	_
	(HappyTerminal (TId happy_var_1))
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_0  7 happyReduction_8
happyReduction_8  =  HappyAbsSyn7
		 ([]
	)

happyReduce_9 = happySpecReduce_2  8 happyReduction_9
happyReduction_9 (HappyAbsSyn7  happy_var_2)
	(HappyTerminal (TId happy_var_1))
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  9 happyReduction_10
happyReduction_10 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_0  9 happyReduction_11
happyReduction_11  =  HappyAbsSyn7
		 ([]
	)

happyReduce_12 = happyReduce 4 10 happyReduction_12
happyReduction_12 ((HappyAbsSyn22  happy_var_4) `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (ModuleStms happy_var_1 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_2  11 happyReduction_13
happyReduction_13 _
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  11 happyReduction_14
happyReduction_14 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_0  11 happyReduction_15
happyReduction_15  =  HappyAbsSyn11
		 ([]
	)

happyReduce_16 = happySpecReduce_3  12 happyReduction_16
happyReduction_16 (HappyAbsSyn7  happy_var_3)
	(HappyAbsSyn26  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (PortDecl happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  13 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn13
		 (Input
	)

happyReduce_18 = happySpecReduce_1  13 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn13
		 (Output
	)

happyReduce_19 = happySpecReduce_1  13 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn13
		 (Inout
	)

happyReduce_20 = happySpecReduce_1  14 happyReduction_20
happyReduction_20 (HappyTerminal (TBinIxDigit happy_var_1))
	 =  HappyAbsSyn14
		 (digit_to_int (BinIxDigit happy_var_1)
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  14 happyReduction_21
happyReduction_21 (HappyTerminal (THexIxDigit happy_var_1))
	 =  HappyAbsSyn14
		 (digit_to_int (HexIxDigit happy_var_1)
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  14 happyReduction_22
happyReduction_22 (HappyTerminal (TDecDigit happy_var_1))
	 =  HappyAbsSyn14
		 (digit_to_int (DecDigit happy_var_1)
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  15 happyReduction_23
happyReduction_23 (HappyTerminal (TBinDigit happy_var_1))
	 =  HappyAbsSyn14
		 (BinIxDigit happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  15 happyReduction_24
happyReduction_24 (HappyTerminal (THexDigit happy_var_1))
	 =  HappyAbsSyn14
		 (HexIxDigit happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  16 happyReduction_25
happyReduction_25 _
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 ([happy_var_1]
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  16 happyReduction_26
happyReduction_26 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1 : happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_0  16 happyReduction_27
happyReduction_27  =  HappyAbsSyn16
		 ([]
	)

happyReduce_28 = happySpecReduce_3  17 happyReduction_28
happyReduction_28 (HappyAbsSyn7  happy_var_3)
	(HappyAbsSyn26  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (Wire happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  18 happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn18
		 (WireTri
	)

happyReduce_30 = happySpecReduce_1  18 happyReduction_30
happyReduction_30 _
	 =  HappyAbsSyn18
		 (WireSimple
	)

happyReduce_31 = happySpecReduce_2  19 happyReduction_31
happyReduction_31 _
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 ([happy_var_1]
	)
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  19 happyReduction_32
happyReduction_32 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1 : happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_0  19 happyReduction_33
happyReduction_33  =  HappyAbsSyn19
		 ([]
	)

happyReduce_34 = happyReduce 4 20 happyReduction_34
happyReduction_34 ((HappyAbsSyn27  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (Parameter happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_35 = happySpecReduce_1  21 happyReduction_35
happyReduction_35 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  21 happyReduction_36
happyReduction_36 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  22 happyReduction_37
happyReduction_37 _
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 ([happy_var_1]
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  22 happyReduction_38
happyReduction_38 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1 : happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_0  22 happyReduction_39
happyReduction_39  =  HappyAbsSyn22
		 ([]
	)

happyReduce_40 = happySpecReduce_1  23 happyReduction_40
happyReduction_40 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  23 happyReduction_41
happyReduction_41 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  23 happyReduction_42
happyReduction_42 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happyReduce 6 24 happyReduction_43
happyReduction_43 ((HappyAbsSyn27  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TId happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 (Defparam happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_44 = happyReduce 4 25 happyReduction_44
happyReduction_44 ((HappyAbsSyn27  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 (Assign happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_45 = happyReduce 5 26 happyReduction_45
happyReduction_45 (_ `HappyStk`
	(HappyAbsSyn27  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn26
		 (Range happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_46 = happySpecReduce_3  26 happyReduction_46
happyReduction_46 _
	(HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (Index happy_var_2
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_0  26 happyReduction_47
happyReduction_47  =  HappyAbsSyn26
		 (EmptyRange
	)

happyReduce_48 = happySpecReduce_2  27 happyReduction_48
happyReduction_48 (HappyAbsSyn26  happy_var_2)
	(HappyTerminal (TId happy_var_1))
	 =  HappyAbsSyn27
		 (E_Var happy_var_1 happy_var_2
	)
happyReduction_48 _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  27 happyReduction_49
happyReduction_49 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn27
		 (E_Constant happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_2  27 happyReduction_50
happyReduction_50 (HappyAbsSyn26  happy_var_2)
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn27
		 (E_ConstantRange happy_var_1 happy_var_2
	)
happyReduction_50 _ _  = notHappyAtAll 

happyReduce_51 = happyReduce 5 27 happyReduction_51
happyReduction_51 ((HappyAbsSyn27  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (E_BitOp $ BE_Select happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_52 = happySpecReduce_3  27 happyReduction_52
happyReduction_52 _
	(HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (E_Union happy_var_2
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_2  27 happyReduction_53
happyReduction_53 (HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (E_BitOp (BE_Neg happy_var_2)
	)
happyReduction_53 _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_3  27 happyReduction_54
happyReduction_54 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (E_BitOp (BE_And happy_var_1 happy_var_3)
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  27 happyReduction_55
happyReduction_55 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (E_BitOp (BE_Or  happy_var_1 happy_var_3)
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  28 happyReduction_56
happyReduction_56 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn28
		 ([happy_var_1]
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  28 happyReduction_57
happyReduction_57 (HappyAbsSyn28  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn28
		 (happy_var_1 : happy_var_3
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_0  28 happyReduction_58
happyReduction_58  =  HappyAbsSyn28
		 ([]
	)

happyReduce_59 = happySpecReduce_1  29 happyReduction_59
happyReduction_59 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn29
		 (C_Digit happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_1  29 happyReduction_60
happyReduction_60 (HappyTerminal (TString happy_var_1))
	 =  HappyAbsSyn29
		 (C_String happy_var_1
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_2  29 happyReduction_61
happyReduction_61 (HappyAbsSyn29  happy_var_2)
	_
	 =  HappyAbsSyn29
		 (C_Neg happy_var_2
	)
happyReduction_61 _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_3  29 happyReduction_62
happyReduction_62 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (C_And happy_var_1 happy_var_3
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_3  29 happyReduction_63
happyReduction_63 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (C_Or happy_var_1 happy_var_3
	)
happyReduction_63 _ _ _  = notHappyAtAll 

happyReduce_64 = happyReduce 5 29 happyReduction_64
happyReduction_64 ((HappyAbsSyn29  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn29
		 (C_Select happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_65 = happySpecReduce_3  30 happyReduction_65
happyReduction_65 (HappyAbsSyn31  happy_var_3)
	(HappyTerminal (TId happy_var_2))
	(HappyTerminal (TId happy_var_1))
	 =  HappyAbsSyn23
		 (Submodule happy_var_1 (Just happy_var_2) happy_var_3
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_2  30 happyReduction_66
happyReduction_66 (HappyAbsSyn31  happy_var_2)
	(HappyTerminal (TId happy_var_1))
	 =  HappyAbsSyn23
		 (Submodule happy_var_1 Nothing happy_var_2
	)
happyReduction_66 _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  31 happyReduction_67
happyReduction_67 _
	(HappyAbsSyn31  happy_var_2)
	_
	 =  HappyAbsSyn31
		 (happy_var_2
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_1  32 happyReduction_68
happyReduction_68 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn31
		 (SubmoduleFlist happy_var_1
	)
happyReduction_68 _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_1  32 happyReduction_69
happyReduction_69 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn31
		 (SubmoduleFlistStrict happy_var_1
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  33 happyReduction_70
happyReduction_70 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn33
		 ([happy_var_1]
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_3  33 happyReduction_71
happyReduction_71 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1 : happy_var_3
	)
happyReduction_71 _ _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_0  33 happyReduction_72
happyReduction_72  =  HappyAbsSyn33
		 ([]
	)

happyReduce_73 = happyReduce 5 34 happyReduction_73
happyReduction_73 (_ `HappyStk`
	(HappyAbsSyn27  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn34
		 ((happy_var_2,happy_var_4)
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 68 68 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TModule -> cont 35;
	TEndmodule -> cont 36;
	TAssign -> cont 37;
	TParameter -> cont 38;
	TDefparam -> cont 39;
	TInput -> cont 40;
	TOutput -> cont 41;
	TInout -> cont 42;
	TTri -> cont 43;
	TWire -> cont 44;
	TParROpen -> cont 45;
	TParRClose -> cont 46;
	TParFOpen -> cont 47;
	TParFClose -> cont 48;
	TParQOpen -> cont 49;
	TParQClose -> cont 50;
	TColon -> cont 51;
	TSemiColon -> cont 52;
	TEqual -> cont 53;
	TComma -> cont 54;
	TPoint -> cont 55;
	TQuest -> cont 56;
	TTilda -> cont 57;
	TAnd -> cont 58;
	TOr -> cont 59;
	TBinDigit happy_dollar_dollar -> cont 60;
	THexDigit happy_dollar_dollar -> cont 61;
	TBinIxDigit happy_dollar_dollar -> cont 62;
	THexIxDigit happy_dollar_dollar -> cont 63;
	TDecDigit happy_dollar_dollar -> cont 64;
	TId happy_dollar_dollar -> cont 65;
	TString happy_dollar_dollar -> cont 66;
	TEOF -> cont 67;
	_ -> happyError' (tk:tks)
	}

happyError_ 68 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => Either String a -> (a -> Either String b) -> Either String b
happyThen = ((>>=))
happyReturn :: () => a -> Either String a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Either String a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(TokenType)] -> Either String a
happyError' = happyError

syntax_parser tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


-- happyError a = Left ("syntax error: " ++ show a)
happyError a = Left (show a)
-- happyError :: [TokenType] -> Either [TokenType] [TokenType]
-- happyError a = Left a

-- alexScanTokens :: String -> Either String [Token]


-- analyze_toks :: [Token] -> Either String [Module]
analyze_toks ts = syntax_parser simple_ts
    where
    simple_ts = map (\(Token _ _ _ _ t) -> t) ts
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates\\GenericTemplate.hs" #-}








{-# LINE 51 "templates\\GenericTemplate.hs" #-}

{-# LINE 61 "templates\\GenericTemplate.hs" #-}

{-# LINE 70 "templates\\GenericTemplate.hs" #-}

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

{-# LINE 148 "templates\\GenericTemplate.hs" #-}

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
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
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
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
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

{-# LINE 312 "templates\\GenericTemplate.hs" #-}
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
