{-# OPTIONS_GHC -w #-}
module Syntax where

import AST
import Lex
import Tokens
import qualified Data.Tree as Tree

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
	| HappyAbsSyn14 (AttributeList)
	| HappyAbsSyn15 ([Attribute])
	| HappyAbsSyn16 (Attribute)
	| HappyAbsSyn17 (Digit)
	| HappyAbsSyn19 ([Wire])
	| HappyAbsSyn20 (Wire)
	| HappyAbsSyn21 (WireType)
	| HappyAbsSyn22 ([Parameter])
	| HappyAbsSyn23 (Parameter)
	| HappyAbsSyn25 ([Stm])
	| HappyAbsSyn26 (Stm)
	| HappyAbsSyn29 (Range)
	| HappyAbsSyn30 (Expr)
	| HappyAbsSyn31 ([Expr])
	| HappyAbsSyn32 (Const)
	| HappyAbsSyn34 (SubmoduleFlist)
	| HappyAbsSyn36 ([(String,Expr)])
	| HappyAbsSyn37 ((String,Expr))

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
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139 :: () => Int -> ({-HappyReduction (Either String) = -}
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
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79 :: () => ({-HappyReduction (Either String) = -}
	   Int 
	-> (TokenType)
	-> HappyState (TokenType) (HappyStk HappyAbsSyn -> [(TokenType)] -> (Either String) HappyAbsSyn)
	-> [HappyState (TokenType) (HappyStk HappyAbsSyn -> [(TokenType)] -> (Either String) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(TokenType)] -> (Either String) HappyAbsSyn)

action_0 (38) = happyReduce_21
action_0 (50) = happyShift action_5
action_0 (4) = happyGoto action_6
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (14) = happyGoto action_4
action_0 _ = happyReduce_4

action_1 (50) = happyShift action_5
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (14) = happyGoto action_4
action_1 _ = happyFail

action_2 (50) = happyShift action_5
action_2 (72) = happyShift action_12
action_2 (6) = happyGoto action_11
action_2 (14) = happyGoto action_4
action_2 _ = happyReduce_21

action_3 _ = happyReduce_2

action_4 (38) = happyShift action_10
action_4 _ = happyFail

action_5 (70) = happyShift action_9
action_5 (15) = happyGoto action_7
action_5 (16) = happyGoto action_8
action_5 _ = happyReduce_24

action_6 (73) = happyAccept
action_6 _ = happyFail

action_7 (51) = happyShift action_15
action_7 (59) = happyShift action_16
action_7 _ = happyFail

action_8 _ = happyReduce_23

action_9 (58) = happyShift action_14
action_9 _ = happyFail

action_10 (70) = happyShift action_13
action_10 _ = happyFail

action_11 _ = happyReduce_3

action_12 _ = happyReduce_1

action_13 (48) = happyShift action_19
action_13 _ = happyFail

action_14 (69) = happyShift action_18
action_14 _ = happyFail

action_15 _ = happyReduce_20

action_16 (70) = happyShift action_9
action_16 (16) = happyGoto action_17
action_16 _ = happyFail

action_17 _ = happyReduce_22

action_18 _ = happyReduce_25

action_19 (70) = happyShift action_21
action_19 (7) = happyGoto action_20
action_19 _ = happyReduce_8

action_20 (49) = happyShift action_23
action_20 _ = happyFail

action_21 (59) = happyShift action_22
action_21 _ = happyReduce_6

action_22 (70) = happyShift action_21
action_22 (7) = happyGoto action_25
action_22 _ = happyReduce_8

action_23 (57) = happyShift action_24
action_23 _ = happyFail

action_24 (43) = happyShift action_30
action_24 (44) = happyShift action_31
action_24 (45) = happyShift action_32
action_24 (10) = happyGoto action_26
action_24 (11) = happyGoto action_27
action_24 (12) = happyGoto action_28
action_24 (13) = happyGoto action_29
action_24 _ = happyReduce_15

action_25 _ = happyReduce_7

action_26 (39) = happyShift action_41
action_26 _ = happyFail

action_27 (46) = happyShift action_39
action_27 (47) = happyShift action_40
action_27 (19) = happyGoto action_36
action_27 (20) = happyGoto action_37
action_27 (21) = happyGoto action_38
action_27 _ = happyReduce_33

action_28 (57) = happyShift action_35
action_28 _ = happyFail

action_29 (54) = happyShift action_34
action_29 (29) = happyGoto action_33
action_29 _ = happyReduce_53

action_30 _ = happyReduce_17

action_31 _ = happyReduce_18

action_32 _ = happyReduce_19

action_33 (70) = happyShift action_63
action_33 (8) = happyGoto action_62
action_33 _ = happyFail

action_34 (52) = happyShift action_53
action_34 (62) = happyShift action_54
action_34 (65) = happyShift action_55
action_34 (66) = happyShift action_56
action_34 (67) = happyShift action_57
action_34 (68) = happyShift action_58
action_34 (69) = happyShift action_59
action_34 (70) = happyShift action_60
action_34 (71) = happyShift action_61
action_34 (17) = happyGoto action_48
action_34 (18) = happyGoto action_49
action_34 (24) = happyGoto action_50
action_34 (30) = happyGoto action_51
action_34 (32) = happyGoto action_52
action_34 _ = happyFail

action_35 (39) = happyReduce_15
action_35 (40) = happyReduce_15
action_35 (41) = happyReduce_15
action_35 (42) = happyReduce_15
action_35 (43) = happyShift action_30
action_35 (44) = happyShift action_31
action_35 (45) = happyShift action_32
action_35 (46) = happyReduce_15
action_35 (47) = happyReduce_15
action_35 (70) = happyReduce_15
action_35 (11) = happyGoto action_47
action_35 (12) = happyGoto action_28
action_35 (13) = happyGoto action_29
action_35 _ = happyReduce_15

action_36 (41) = happyShift action_46
action_36 (22) = happyGoto action_44
action_36 (23) = happyGoto action_45
action_36 _ = happyReduce_39

action_37 (57) = happyShift action_43
action_37 _ = happyFail

action_38 (54) = happyShift action_34
action_38 (29) = happyGoto action_42
action_38 _ = happyReduce_53

action_39 _ = happyReduce_35

action_40 _ = happyReduce_36

action_41 _ = happyReduce_5

action_42 (70) = happyShift action_63
action_42 (8) = happyGoto action_91
action_42 _ = happyFail

action_43 (39) = happyReduce_33
action_43 (40) = happyReduce_33
action_43 (41) = happyReduce_33
action_43 (42) = happyReduce_33
action_43 (46) = happyShift action_39
action_43 (47) = happyShift action_40
action_43 (70) = happyReduce_33
action_43 (19) = happyGoto action_90
action_43 (20) = happyGoto action_37
action_43 (21) = happyGoto action_38
action_43 _ = happyReduce_33

action_44 (40) = happyShift action_87
action_44 (42) = happyShift action_88
action_44 (70) = happyShift action_89
action_44 (25) = happyGoto action_82
action_44 (26) = happyGoto action_83
action_44 (27) = happyGoto action_84
action_44 (28) = happyGoto action_85
action_44 (33) = happyGoto action_86
action_44 _ = happyReduce_45

action_45 (57) = happyShift action_81
action_45 _ = happyFail

action_46 (70) = happyShift action_80
action_46 _ = happyFail

action_47 _ = happyReduce_14

action_48 _ = happyReduce_41

action_49 _ = happyReduce_42

action_50 _ = happyReduce_65

action_51 (55) = happyShift action_75
action_51 (56) = happyShift action_76
action_51 (61) = happyShift action_77
action_51 (63) = happyShift action_78
action_51 (64) = happyShift action_79
action_51 _ = happyFail

action_52 (49) = happyReduce_55
action_52 (53) = happyReduce_55
action_52 (54) = happyShift action_34
action_52 (55) = happyReduce_55
action_52 (56) = happyReduce_55
action_52 (57) = happyReduce_55
action_52 (58) = happyReduce_55
action_52 (59) = happyReduce_55
action_52 (61) = happyShift action_72
action_52 (63) = happyShift action_73
action_52 (64) = happyShift action_74
action_52 (29) = happyGoto action_71
action_52 _ = happyReduce_55

action_53 (52) = happyShift action_53
action_53 (62) = happyShift action_54
action_53 (65) = happyShift action_55
action_53 (66) = happyShift action_56
action_53 (67) = happyShift action_57
action_53 (68) = happyShift action_58
action_53 (69) = happyShift action_59
action_53 (70) = happyShift action_60
action_53 (71) = happyShift action_61
action_53 (17) = happyGoto action_48
action_53 (18) = happyGoto action_49
action_53 (24) = happyGoto action_50
action_53 (30) = happyGoto action_69
action_53 (31) = happyGoto action_70
action_53 (32) = happyGoto action_52
action_53 _ = happyReduce_64

action_54 (52) = happyShift action_53
action_54 (62) = happyShift action_54
action_54 (65) = happyShift action_55
action_54 (66) = happyShift action_56
action_54 (67) = happyShift action_57
action_54 (68) = happyShift action_58
action_54 (69) = happyShift action_59
action_54 (70) = happyShift action_60
action_54 (71) = happyShift action_61
action_54 (17) = happyGoto action_48
action_54 (18) = happyGoto action_49
action_54 (24) = happyGoto action_50
action_54 (30) = happyGoto action_67
action_54 (32) = happyGoto action_68
action_54 _ = happyFail

action_55 _ = happyReduce_29

action_56 _ = happyReduce_30

action_57 _ = happyReduce_26

action_58 _ = happyReduce_27

action_59 _ = happyReduce_28

action_60 (54) = happyShift action_34
action_60 (29) = happyGoto action_66
action_60 _ = happyReduce_53

action_61 _ = happyReduce_66

action_62 _ = happyReduce_16

action_63 (59) = happyShift action_65
action_63 (9) = happyGoto action_64
action_63 _ = happyReduce_11

action_64 _ = happyReduce_9

action_65 (70) = happyShift action_63
action_65 (8) = happyGoto action_110
action_65 _ = happyFail

action_66 _ = happyReduce_54

action_67 (61) = happyShift action_77
action_67 (63) = happyShift action_78
action_67 (64) = happyShift action_79
action_67 _ = happyReduce_59

action_68 (49) = happyReduce_67
action_68 (53) = happyReduce_67
action_68 (54) = happyShift action_34
action_68 (55) = happyReduce_67
action_68 (56) = happyReduce_67
action_68 (57) = happyReduce_67
action_68 (58) = happyReduce_67
action_68 (59) = happyReduce_67
action_68 (61) = happyShift action_72
action_68 (63) = happyShift action_73
action_68 (64) = happyShift action_74
action_68 (29) = happyGoto action_71
action_68 _ = happyReduce_67

action_69 (59) = happyShift action_109
action_69 (61) = happyShift action_77
action_69 (63) = happyShift action_78
action_69 (64) = happyShift action_79
action_69 _ = happyReduce_62

action_70 (53) = happyShift action_108
action_70 _ = happyFail

action_71 _ = happyReduce_56

action_72 (62) = happyShift action_105
action_72 (65) = happyShift action_55
action_72 (66) = happyShift action_56
action_72 (67) = happyShift action_57
action_72 (68) = happyShift action_58
action_72 (69) = happyShift action_59
action_72 (71) = happyShift action_61
action_72 (17) = happyGoto action_48
action_72 (18) = happyGoto action_49
action_72 (24) = happyGoto action_50
action_72 (32) = happyGoto action_107
action_72 _ = happyFail

action_73 (62) = happyShift action_105
action_73 (65) = happyShift action_55
action_73 (66) = happyShift action_56
action_73 (67) = happyShift action_57
action_73 (68) = happyShift action_58
action_73 (69) = happyShift action_59
action_73 (71) = happyShift action_61
action_73 (17) = happyGoto action_48
action_73 (18) = happyGoto action_49
action_73 (24) = happyGoto action_50
action_73 (32) = happyGoto action_106
action_73 _ = happyFail

action_74 (62) = happyShift action_105
action_74 (65) = happyShift action_55
action_74 (66) = happyShift action_56
action_74 (67) = happyShift action_57
action_74 (68) = happyShift action_58
action_74 (69) = happyShift action_59
action_74 (71) = happyShift action_61
action_74 (17) = happyGoto action_48
action_74 (18) = happyGoto action_49
action_74 (24) = happyGoto action_50
action_74 (32) = happyGoto action_104
action_74 _ = happyFail

action_75 _ = happyReduce_52

action_76 (52) = happyShift action_53
action_76 (62) = happyShift action_54
action_76 (65) = happyShift action_55
action_76 (66) = happyShift action_56
action_76 (67) = happyShift action_57
action_76 (68) = happyShift action_58
action_76 (69) = happyShift action_59
action_76 (70) = happyShift action_60
action_76 (71) = happyShift action_61
action_76 (17) = happyGoto action_48
action_76 (18) = happyGoto action_49
action_76 (24) = happyGoto action_50
action_76 (30) = happyGoto action_103
action_76 (32) = happyGoto action_52
action_76 _ = happyFail

action_77 (52) = happyShift action_53
action_77 (62) = happyShift action_54
action_77 (65) = happyShift action_55
action_77 (66) = happyShift action_56
action_77 (67) = happyShift action_57
action_77 (68) = happyShift action_58
action_77 (69) = happyShift action_59
action_77 (70) = happyShift action_60
action_77 (71) = happyShift action_61
action_77 (17) = happyGoto action_48
action_77 (18) = happyGoto action_49
action_77 (24) = happyGoto action_50
action_77 (30) = happyGoto action_102
action_77 (32) = happyGoto action_52
action_77 _ = happyFail

action_78 (52) = happyShift action_53
action_78 (62) = happyShift action_54
action_78 (65) = happyShift action_55
action_78 (66) = happyShift action_56
action_78 (67) = happyShift action_57
action_78 (68) = happyShift action_58
action_78 (69) = happyShift action_59
action_78 (70) = happyShift action_60
action_78 (71) = happyShift action_61
action_78 (17) = happyGoto action_48
action_78 (18) = happyGoto action_49
action_78 (24) = happyGoto action_50
action_78 (30) = happyGoto action_101
action_78 (32) = happyGoto action_52
action_78 _ = happyFail

action_79 (52) = happyShift action_53
action_79 (62) = happyShift action_54
action_79 (65) = happyShift action_55
action_79 (66) = happyShift action_56
action_79 (67) = happyShift action_57
action_79 (68) = happyShift action_58
action_79 (69) = happyShift action_59
action_79 (70) = happyShift action_60
action_79 (71) = happyShift action_61
action_79 (17) = happyGoto action_48
action_79 (18) = happyGoto action_49
action_79 (24) = happyGoto action_50
action_79 (30) = happyGoto action_100
action_79 (32) = happyGoto action_52
action_79 _ = happyFail

action_80 (58) = happyShift action_99
action_80 _ = happyFail

action_81 (39) = happyReduce_39
action_81 (40) = happyReduce_39
action_81 (41) = happyShift action_46
action_81 (42) = happyReduce_39
action_81 (70) = happyReduce_39
action_81 (22) = happyGoto action_98
action_81 (23) = happyGoto action_45
action_81 _ = happyReduce_39

action_82 _ = happyReduce_12

action_83 (57) = happyShift action_97
action_83 _ = happyFail

action_84 _ = happyReduce_48

action_85 _ = happyReduce_46

action_86 _ = happyReduce_47

action_87 (50) = happyShift action_5
action_87 (14) = happyGoto action_96
action_87 _ = happyReduce_21

action_88 (70) = happyShift action_95
action_88 _ = happyFail

action_89 (48) = happyShift action_93
action_89 (70) = happyShift action_94
action_89 (34) = happyGoto action_92
action_89 _ = happyFail

action_90 _ = happyReduce_32

action_91 _ = happyReduce_34

action_92 _ = happyReduce_72

action_93 (49) = happyReduce_78
action_93 (52) = happyShift action_53
action_93 (60) = happyShift action_125
action_93 (62) = happyShift action_54
action_93 (65) = happyShift action_55
action_93 (66) = happyShift action_56
action_93 (67) = happyShift action_57
action_93 (68) = happyShift action_58
action_93 (69) = happyShift action_59
action_93 (70) = happyShift action_60
action_93 (71) = happyShift action_61
action_93 (17) = happyGoto action_48
action_93 (18) = happyGoto action_49
action_93 (24) = happyGoto action_50
action_93 (30) = happyGoto action_69
action_93 (31) = happyGoto action_121
action_93 (32) = happyGoto action_52
action_93 (35) = happyGoto action_122
action_93 (36) = happyGoto action_123
action_93 (37) = happyGoto action_124
action_93 _ = happyReduce_78

action_94 (48) = happyShift action_93
action_94 (34) = happyGoto action_120
action_94 _ = happyFail

action_95 (60) = happyShift action_119
action_95 _ = happyFail

action_96 (52) = happyShift action_53
action_96 (62) = happyShift action_54
action_96 (65) = happyShift action_55
action_96 (66) = happyShift action_56
action_96 (67) = happyShift action_57
action_96 (68) = happyShift action_58
action_96 (69) = happyShift action_59
action_96 (70) = happyShift action_60
action_96 (71) = happyShift action_61
action_96 (17) = happyGoto action_48
action_96 (18) = happyGoto action_49
action_96 (24) = happyGoto action_50
action_96 (30) = happyGoto action_118
action_96 (32) = happyGoto action_52
action_96 _ = happyFail

action_97 (39) = happyReduce_45
action_97 (40) = happyShift action_87
action_97 (42) = happyShift action_88
action_97 (70) = happyShift action_89
action_97 (25) = happyGoto action_117
action_97 (26) = happyGoto action_83
action_97 (27) = happyGoto action_84
action_97 (28) = happyGoto action_85
action_97 (33) = happyGoto action_86
action_97 _ = happyReduce_45

action_98 _ = happyReduce_38

action_99 (52) = happyShift action_53
action_99 (62) = happyShift action_54
action_99 (65) = happyShift action_55
action_99 (66) = happyShift action_56
action_99 (67) = happyShift action_57
action_99 (68) = happyShift action_58
action_99 (69) = happyShift action_59
action_99 (70) = happyShift action_60
action_99 (71) = happyShift action_61
action_99 (17) = happyGoto action_48
action_99 (18) = happyGoto action_49
action_99 (24) = happyGoto action_50
action_99 (30) = happyGoto action_116
action_99 (32) = happyGoto action_52
action_99 _ = happyFail

action_100 (61) = happyShift action_77
action_100 _ = happyReduce_61

action_101 (61) = happyShift action_77
action_101 (64) = happyShift action_79
action_101 _ = happyReduce_60

action_102 (56) = happyShift action_115
action_102 (61) = happyShift action_77
action_102 (63) = happyShift action_78
action_102 (64) = happyShift action_79
action_102 _ = happyFail

action_103 (55) = happyShift action_114
action_103 (61) = happyShift action_77
action_103 (63) = happyShift action_78
action_103 (64) = happyShift action_79
action_103 _ = happyFail

action_104 (61) = happyShift action_72
action_104 _ = happyReduce_69

action_105 (62) = happyShift action_105
action_105 (65) = happyShift action_55
action_105 (66) = happyShift action_56
action_105 (67) = happyShift action_57
action_105 (68) = happyShift action_58
action_105 (69) = happyShift action_59
action_105 (71) = happyShift action_61
action_105 (17) = happyGoto action_48
action_105 (18) = happyGoto action_49
action_105 (24) = happyGoto action_50
action_105 (32) = happyGoto action_113
action_105 _ = happyFail

action_106 (61) = happyShift action_72
action_106 (64) = happyShift action_74
action_106 _ = happyReduce_68

action_107 (56) = happyShift action_112
action_107 (61) = happyShift action_72
action_107 (63) = happyShift action_73
action_107 (64) = happyShift action_74
action_107 _ = happyFail

action_108 _ = happyReduce_58

action_109 (52) = happyShift action_53
action_109 (62) = happyShift action_54
action_109 (65) = happyShift action_55
action_109 (66) = happyShift action_56
action_109 (67) = happyShift action_57
action_109 (68) = happyShift action_58
action_109 (69) = happyShift action_59
action_109 (70) = happyShift action_60
action_109 (71) = happyShift action_61
action_109 (17) = happyGoto action_48
action_109 (18) = happyGoto action_49
action_109 (24) = happyGoto action_50
action_109 (30) = happyGoto action_69
action_109 (31) = happyGoto action_111
action_109 (32) = happyGoto action_52
action_109 _ = happyReduce_64

action_110 _ = happyReduce_10

action_111 _ = happyReduce_63

action_112 (62) = happyShift action_105
action_112 (65) = happyShift action_55
action_112 (66) = happyShift action_56
action_112 (67) = happyShift action_57
action_112 (68) = happyShift action_58
action_112 (69) = happyShift action_59
action_112 (71) = happyShift action_61
action_112 (17) = happyGoto action_48
action_112 (18) = happyGoto action_49
action_112 (24) = happyGoto action_50
action_112 (32) = happyGoto action_132
action_112 _ = happyFail

action_113 (61) = happyShift action_72
action_113 (63) = happyShift action_73
action_113 (64) = happyShift action_74
action_113 _ = happyReduce_67

action_114 _ = happyReduce_51

action_115 (52) = happyShift action_53
action_115 (62) = happyShift action_54
action_115 (65) = happyShift action_55
action_115 (66) = happyShift action_56
action_115 (67) = happyShift action_57
action_115 (68) = happyShift action_58
action_115 (69) = happyShift action_59
action_115 (70) = happyShift action_60
action_115 (71) = happyShift action_61
action_115 (17) = happyGoto action_48
action_115 (18) = happyGoto action_49
action_115 (24) = happyGoto action_50
action_115 (30) = happyGoto action_131
action_115 (32) = happyGoto action_52
action_115 _ = happyFail

action_116 (61) = happyShift action_77
action_116 (63) = happyShift action_78
action_116 (64) = happyShift action_79
action_116 _ = happyReduce_40

action_117 _ = happyReduce_44

action_118 (58) = happyShift action_130
action_118 (61) = happyShift action_77
action_118 (63) = happyShift action_78
action_118 (64) = happyShift action_79
action_118 _ = happyFail

action_119 (70) = happyShift action_129
action_119 _ = happyFail

action_120 _ = happyReduce_71

action_121 _ = happyReduce_74

action_122 (49) = happyShift action_128
action_122 _ = happyFail

action_123 _ = happyReduce_75

action_124 (59) = happyShift action_127
action_124 _ = happyReduce_76

action_125 (70) = happyShift action_126
action_125 _ = happyFail

action_126 (48) = happyShift action_136
action_126 _ = happyFail

action_127 (60) = happyShift action_125
action_127 (36) = happyGoto action_135
action_127 (37) = happyGoto action_124
action_127 _ = happyReduce_78

action_128 _ = happyReduce_73

action_129 (58) = happyShift action_134
action_129 _ = happyFail

action_130 (52) = happyShift action_53
action_130 (62) = happyShift action_54
action_130 (65) = happyShift action_55
action_130 (66) = happyShift action_56
action_130 (67) = happyShift action_57
action_130 (68) = happyShift action_58
action_130 (69) = happyShift action_59
action_130 (70) = happyShift action_60
action_130 (71) = happyShift action_61
action_130 (17) = happyGoto action_48
action_130 (18) = happyGoto action_49
action_130 (24) = happyGoto action_50
action_130 (30) = happyGoto action_133
action_130 (32) = happyGoto action_52
action_130 _ = happyFail

action_131 _ = happyReduce_57

action_132 _ = happyReduce_70

action_133 (61) = happyShift action_77
action_133 (63) = happyShift action_78
action_133 (64) = happyShift action_79
action_133 _ = happyReduce_50

action_134 (52) = happyShift action_53
action_134 (62) = happyShift action_54
action_134 (65) = happyShift action_55
action_134 (66) = happyShift action_56
action_134 (67) = happyShift action_57
action_134 (68) = happyShift action_58
action_134 (69) = happyShift action_59
action_134 (70) = happyShift action_60
action_134 (71) = happyShift action_61
action_134 (17) = happyGoto action_48
action_134 (18) = happyGoto action_49
action_134 (24) = happyGoto action_50
action_134 (30) = happyGoto action_138
action_134 (32) = happyGoto action_52
action_134 _ = happyFail

action_135 _ = happyReduce_77

action_136 (52) = happyShift action_53
action_136 (62) = happyShift action_54
action_136 (65) = happyShift action_55
action_136 (66) = happyShift action_56
action_136 (67) = happyShift action_57
action_136 (68) = happyShift action_58
action_136 (69) = happyShift action_59
action_136 (70) = happyShift action_60
action_136 (71) = happyShift action_61
action_136 (17) = happyGoto action_48
action_136 (18) = happyGoto action_49
action_136 (24) = happyGoto action_50
action_136 (30) = happyGoto action_137
action_136 (32) = happyGoto action_52
action_136 _ = happyFail

action_137 (49) = happyShift action_139
action_137 (61) = happyShift action_77
action_137 (63) = happyShift action_78
action_137 (64) = happyShift action_79
action_137 _ = happyFail

action_138 (61) = happyShift action_77
action_138 (63) = happyShift action_78
action_138 (64) = happyShift action_79
action_138 _ = happyReduce_49

action_139 _ = happyReduce_79

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 _
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1]
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_2 : happy_var_1
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_0  5 happyReduction_4
happyReduction_4  =  HappyAbsSyn4
		 ([]
	)

happyReduce_5 = happyReduce 9 6 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TId happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Module happy_var_1 happy_var_3 happy_var_5 happy_var_8
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
happyReduction_12 ((HappyAbsSyn25  happy_var_4) `HappyStk`
	(HappyAbsSyn22  happy_var_3) `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
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
	(HappyAbsSyn29  happy_var_2)
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

happyReduce_20 = happySpecReduce_3  14 happyReduction_20
happyReduction_20 _
	(HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (AttributeList happy_var_2
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_0  14 happyReduction_21
happyReduction_21  =  HappyAbsSyn14
		 (AttributeList []
	)

happyReduce_22 = happySpecReduce_3  15 happyReduction_22
happyReduction_22 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_3 : happy_var_1
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  15 happyReduction_23
happyReduction_23 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 ([happy_var_1]
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_0  15 happyReduction_24
happyReduction_24  =  HappyAbsSyn15
		 ([]
	)

happyReduce_25 = happySpecReduce_3  16 happyReduction_25
happyReduction_25 (HappyTerminal (TDecDigit happy_var_3))
	_
	(HappyTerminal (TId happy_var_1))
	 =  HappyAbsSyn16
		 (Attribute happy_var_1 (read happy_var_3)
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  17 happyReduction_26
happyReduction_26 (HappyTerminal (TBinIxDigit happy_var_1))
	 =  HappyAbsSyn17
		 (digit_to_int (BinIxDigit happy_var_1)
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  17 happyReduction_27
happyReduction_27 (HappyTerminal (THexIxDigit happy_var_1))
	 =  HappyAbsSyn17
		 (digit_to_int (HexIxDigit happy_var_1)
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  17 happyReduction_28
happyReduction_28 (HappyTerminal (TDecDigit happy_var_1))
	 =  HappyAbsSyn17
		 (digit_to_int (DecDigit happy_var_1)
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  18 happyReduction_29
happyReduction_29 (HappyTerminal (TBinDigit happy_var_1))
	 =  HappyAbsSyn17
		 (BinIxDigit happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  18 happyReduction_30
happyReduction_30 (HappyTerminal (THexDigit happy_var_1))
	 =  HappyAbsSyn17
		 (HexIxDigit happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

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

happyReduce_34 = happySpecReduce_3  20 happyReduction_34
happyReduction_34 (HappyAbsSyn7  happy_var_3)
	(HappyAbsSyn29  happy_var_2)
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn20
		 (Wire happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  21 happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn21
		 (WireTri
	)

happyReduce_36 = happySpecReduce_1  21 happyReduction_36
happyReduction_36 _
	 =  HappyAbsSyn21
		 (WireSimple
	)

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

happyReduce_40 = happyReduce 4 23 happyReduction_40
happyReduction_40 ((HappyAbsSyn30  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 (Parameter happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_41 = happySpecReduce_1  24 happyReduction_41
happyReduction_41 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  24 happyReduction_42
happyReduction_42 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_2  25 happyReduction_43
happyReduction_43 _
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn25
		 ([happy_var_1]
	)
happyReduction_43 _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  25 happyReduction_44
happyReduction_44 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1 : happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_0  25 happyReduction_45
happyReduction_45  =  HappyAbsSyn25
		 ([]
	)

happyReduce_46 = happySpecReduce_1  26 happyReduction_46
happyReduction_46 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  26 happyReduction_47
happyReduction_47 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  26 happyReduction_48
happyReduction_48 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happyReduce 6 27 happyReduction_49
happyReduction_49 ((HappyAbsSyn30  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TId happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn26
		 (Defparam happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_50 = happyReduce 5 28 happyReduction_50
happyReduction_50 ((HappyAbsSyn30  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_3) `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn26
		 (Assign happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_51 = happyReduce 5 29 happyReduction_51
happyReduction_51 (_ `HappyStk`
	(HappyAbsSyn30  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn29
		 (Range happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_52 = happySpecReduce_3  29 happyReduction_52
happyReduction_52 _
	(HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn29
		 (Index happy_var_2
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_0  29 happyReduction_53
happyReduction_53  =  HappyAbsSyn29
		 (EmptyRange
	)

happyReduce_54 = happySpecReduce_2  30 happyReduction_54
happyReduction_54 (HappyAbsSyn29  happy_var_2)
	(HappyTerminal (TId happy_var_1))
	 =  HappyAbsSyn30
		 (E_Var happy_var_1 happy_var_2
	)
happyReduction_54 _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  30 happyReduction_55
happyReduction_55 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn30
		 (E_Constant happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_2  30 happyReduction_56
happyReduction_56 (HappyAbsSyn29  happy_var_2)
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn30
		 (E_ConstantRange happy_var_1 happy_var_2
	)
happyReduction_56 _ _  = notHappyAtAll 

happyReduce_57 = happyReduce 5 30 happyReduction_57
happyReduction_57 ((HappyAbsSyn30  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn30
		 (E_BitOp $ BE_Select happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_58 = happySpecReduce_3  30 happyReduction_58
happyReduction_58 _
	(HappyAbsSyn31  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (E_Union happy_var_2
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_2  30 happyReduction_59
happyReduction_59 (HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (E_BitOp (BE_Neg happy_var_2)
	)
happyReduction_59 _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  30 happyReduction_60
happyReduction_60 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (E_BitOp (BE_And happy_var_1 happy_var_3)
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_3  30 happyReduction_61
happyReduction_61 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (E_BitOp (BE_Or  happy_var_1 happy_var_3)
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  31 happyReduction_62
happyReduction_62 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn31
		 ([happy_var_1]
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_3  31 happyReduction_63
happyReduction_63 (HappyAbsSyn31  happy_var_3)
	_
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1 : happy_var_3
	)
happyReduction_63 _ _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_0  31 happyReduction_64
happyReduction_64  =  HappyAbsSyn31
		 ([]
	)

happyReduce_65 = happySpecReduce_1  32 happyReduction_65
happyReduction_65 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn32
		 (C_Digit happy_var_1
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  32 happyReduction_66
happyReduction_66 (HappyTerminal (TString happy_var_1))
	 =  HappyAbsSyn32
		 (C_String happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_2  32 happyReduction_67
happyReduction_67 (HappyAbsSyn32  happy_var_2)
	_
	 =  HappyAbsSyn32
		 (C_Neg happy_var_2
	)
happyReduction_67 _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  32 happyReduction_68
happyReduction_68 (HappyAbsSyn32  happy_var_3)
	_
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn32
		 (C_And happy_var_1 happy_var_3
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_3  32 happyReduction_69
happyReduction_69 (HappyAbsSyn32  happy_var_3)
	_
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn32
		 (C_Or happy_var_1 happy_var_3
	)
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happyReduce 5 32 happyReduction_70
happyReduction_70 ((HappyAbsSyn32  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn32  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn32  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn32
		 (C_Select happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_71 = happySpecReduce_3  33 happyReduction_71
happyReduction_71 (HappyAbsSyn34  happy_var_3)
	(HappyTerminal (TId happy_var_2))
	(HappyTerminal (TId happy_var_1))
	 =  HappyAbsSyn26
		 (Submodule happy_var_1 (Just happy_var_2) happy_var_3
	)
happyReduction_71 _ _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_2  33 happyReduction_72
happyReduction_72 (HappyAbsSyn34  happy_var_2)
	(HappyTerminal (TId happy_var_1))
	 =  HappyAbsSyn26
		 (Submodule happy_var_1 Nothing happy_var_2
	)
happyReduction_72 _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_3  34 happyReduction_73
happyReduction_73 _
	(HappyAbsSyn34  happy_var_2)
	_
	 =  HappyAbsSyn34
		 (happy_var_2
	)
happyReduction_73 _ _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  35 happyReduction_74
happyReduction_74 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn34
		 (SubmoduleFlist happy_var_1
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1  35 happyReduction_75
happyReduction_75 (HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn34
		 (SubmoduleFlistStrict happy_var_1
	)
happyReduction_75 _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  36 happyReduction_76
happyReduction_76 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn36
		 ([happy_var_1]
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_3  36 happyReduction_77
happyReduction_77 (HappyAbsSyn36  happy_var_3)
	_
	(HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn36
		 (happy_var_1 : happy_var_3
	)
happyReduction_77 _ _ _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_0  36 happyReduction_78
happyReduction_78  =  HappyAbsSyn36
		 ([]
	)

happyReduce_79 = happyReduce 5 37 happyReduction_79
happyReduction_79 (_ `HappyStk`
	(HappyAbsSyn30  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn37
		 ((happy_var_2,happy_var_4)
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 73 73 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TModule -> cont 38;
	TEndmodule -> cont 39;
	TAssign -> cont 40;
	TParameter -> cont 41;
	TDefparam -> cont 42;
	TInput -> cont 43;
	TOutput -> cont 44;
	TInout -> cont 45;
	TTri -> cont 46;
	TWire -> cont 47;
	TParROpen -> cont 48;
	TParRClose -> cont 49;
	TAttrBegin -> cont 50;
	TAttrEnd -> cont 51;
	TParFOpen -> cont 52;
	TParFClose -> cont 53;
	TParQOpen -> cont 54;
	TParQClose -> cont 55;
	TColon -> cont 56;
	TSemiColon -> cont 57;
	TEqual -> cont 58;
	TComma -> cont 59;
	TPoint -> cont 60;
	TQuest -> cont 61;
	TTilda -> cont 62;
	TAnd -> cont 63;
	TOr -> cont 64;
	TBinDigit happy_dollar_dollar -> cont 65;
	THexDigit happy_dollar_dollar -> cont 66;
	TBinIxDigit happy_dollar_dollar -> cont 67;
	THexIxDigit happy_dollar_dollar -> cont 68;
	TDecDigit happy_dollar_dollar -> cont 69;
	TId happy_dollar_dollar -> cont 70;
	TString happy_dollar_dollar -> cont 71;
	TEOF -> cont 72;
	_ -> happyError' (tk:tks)
	}

happyError_ 73 tk tks = happyError' tks
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
