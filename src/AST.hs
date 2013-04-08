{-# LANGUAGE DeriveFunctor #-}
--
-- Author: Ivan
-- File: AST.hs
-- Data: 2013-02-14

module AST where

import Data.List
import Data.Tree(Tree)
import qualified Data.Tree as Tree

class ASTPrinter a where
    print_struct :: a -> String

data File = File { file_name :: FilePath, file_modules :: [Module] }
    deriving(Eq,Ord,Show,Read)

type AST = Tree File

-- name, port_def, mstms
data Module = Module String [String] ModuleStms
    deriving(Eq,Ord,Show,Read)

print_ast :: [Module] -> String
print_ast = print_modules -- Tree.drawTree . fmap (print_modules . file_modules )
    where
    print_modules = concat . (intersperse "\n") . (map print_struct)

print_struct_varlist :: [String] -> String
print_struct_varlist = concat . (intersperse ",")

module_name      (Module name _  _ )  = name
module_portdef   (Module _   pd  _ )  = pd

module_portdecl  (Module _ _  (ModuleStms pd w pr stms  ) ) = pd
module_wires     (Module _ _  (ModuleStms pd w pr stms  ) ) = w
module_params    (Module _ _  (ModuleStms pd w pr stms  ) ) = pr
module_stms      (Module _ _  (ModuleStms pd w pr stms  ) ) = stms

instance ASTPrinter Module where
    print_struct (Module name portdef mstms) = 
        "module " ++ name ++ "(\n" ++ (print_struct_varlist portdef) ++ "\n);\n" ++
        module_stms ++
        "endmodule\n"
        where
        module_stms = print_struct mstms

data ModuleStms = ModuleStms
    [PortDecl]  -- module_stm_ports_decl
    [Wire]      -- module_stm_wires_decl
    [Parameter] -- module_stm_parameters
    [Stm]       -- module_stm_stms
    deriving(Eq,Ord,Show,Read)

instance ASTPrinter ModuleStms where
    print_struct (ModuleStms ports wires params stms) = 
        let p = map print_struct' ports
            w = map print_struct' wires
            pa = map print_struct' params
            s = map print_struct' stms
         in unlines (p ++ w ++ pa ++ s)
		where
		print_struct' x = (print_struct x) ++ "// " ++ (show x)
----------------------------------------------------------------
--

data Digit = 
    BinDigit String | 
    HexDigit String |
    
    BinIxDigit String | 
    HexIxDigit String |
    
    DecDigit String
    deriving(Eq,Ord,Show,Read)

instance ASTPrinter Digit where
    print_struct (BinDigit s) = s
    print_struct (HexDigit s) = s

    print_struct (BinIxDigit s) = s
    print_struct (HexIxDigit s) = s
    print_struct (DecDigit s)  = s
    -- print_struct        _     = undefined

digit_to_int :: Digit -> Digit
digit_to_int = id

data Range = 
    Range Expr Expr |
    Index Expr |
    EmptyRange
    deriving(Eq,Ord,Show,Read)

instance ASTPrinter Range where
    print_struct (Range b e) = "[" ++ (print_struct b) ++ ":" ++ (print_struct e) ++ "]"
    print_struct (Index i  ) = "[" ++ (print_struct i) ++ "]"
    print_struct (EmptyRange) = ""

data BitExpr e 
    = BE_And e e
    | BE_Or e e
    | BE_Select e e e
    | BE_Neg e
--    BE_Xor e e
    deriving(Eq,Ord,Show,Read,Functor)
{-
instance Functor BitExpr where
    fmap f (BE_And a b) = BE_And (f a) (f b)
    fmap f (BE_Or a b)  = BE_Or (f a) (f b)
    fmap f (BE_Select s a b) = BE_Select (f s) (f a) (f b)
    fmap f (BE_Neg a)   = BE_Neg (f a)
-}

data Expr = 
    E_Constant Const |
    E_ConstantRange Const Range |
    E_Var String Range |
    E_Union [Expr] |
    E_BitOp (BitExpr Expr)
    deriving(Eq,Ord,Show,Read)

{-
data Expr = 
    E_Constant Const |
    E_ConstantRange Const Range |
    E_Var String Range |
    E_Neg Expr |
    E_And Expr Expr |
    E_Or Expr Expr |
    E_Select Expr Expr Expr |
    E_Union [Expr]
    deriving(Eq,Ord,Show,Read)
-}

instance (ASTPrinter e) => ASTPrinter (BitExpr e) where
    print_struct (BE_And a b)    = (print_struct a) ++ " & " ++ (print_struct b)
    print_struct (BE_Or  a b)    = (print_struct a) ++ " | " ++ (print_struct b)
    print_struct (BE_Neg a  )    = "~" ++ (print_struct a)
    print_struct (BE_Select s a b)    = (print_struct s) ++ " ? " ++ (print_struct a) ++ " : " ++ (print_struct b)

instance ASTPrinter Expr where
    print_struct (E_Constant c) = print_struct c
    print_struct (E_Var i r)    = i ++ (print_struct r)
{-
    print_struct (E_And a b)    = (print_struct a) ++ " & " ++ (print_struct b)
    print_struct (E_Or  a b)    = (print_struct a) ++ " | " ++ (print_struct b)
    print_struct (E_Neg a  )    = "~" ++ (print_struct a)
    print_struct (E_Select s a b)    = (print_struct s) ++ " ? " ++ (print_struct a) ++ " : " ++ (print_struct b)
-}

    print_struct (E_Union exs)  = "{" ++ (concat $ intersperse "," $ map print_struct exs) ++ "}"
    print_struct (E_BitOp op)   = print_struct op
    print_struct x              = "<><><><><><><><>" ++ show x ++ "<><><><><><><>"

-- consts

data PortName = 
    PortName String Range |
    PortName_Union [PortName]
    deriving(Eq,Ord,Show,Read)

{-
data CRange = 
    CRange Const Const
    | CIndex Const
    | EmptyCRange
    deriving(Eq,Ord,Show,Read)

instance ASTPrinter CRange where
    print_struct (CRange b e) = "[" ++ (print_struct b) ++ ":" ++ (print_struct e) ++ "]"
    print_struct (CIndex i  ) = "[" ++ (print_struct i) ++ "]"
    print_struct (EmptyCRange) = ""
-}

data Const = 
    C_String String |
    C_Digit Digit   |
    C_Neg Const |
    C_And Const Const |
    C_Or Const Const |
    C_Select Const Const Const
    deriving(Eq,Ord,Show,Read)

instance ASTPrinter Const where
    print_struct (C_String s )  = s
    print_struct (C_Digit d  )  = print_struct d
    
    print_struct (C_And a b)    = (print_struct a) ++ " & " ++ (print_struct b)
    print_struct (C_Or  a b)    = (print_struct a) ++ " | " ++ (print_struct b)
    print_struct (C_Neg a  )    = "~" ++ (print_struct a)
    print_struct (C_Select s a b)    = (print_struct s) ++ " ? " ++ (print_struct a) ++ " : " ++ (print_struct b)

----------------------------------------------------------------
-- Port
data Direction = Input | Output | Inout
    deriving(Eq,Ord,Show,Read)

instance ASTPrinter Direction where
    print_struct Input = "input"
    print_struct Output = "output"
    print_struct Inout  = "inout"

data PortType = PortTri | EmptyPortType
    deriving(Eq,Ord,Show,Read)

instance ASTPrinter PortType where
    print_struct PortTri = "tri"
    print_struct EmptyPortType = ""

data PortDecl = 
    PortDecl Direction Range [String]
    deriving(Eq,Ord,Show,Read)

instance ASTPrinter PortDecl where
    print_struct (PortDecl dir r names) = 
        print_struct dir ++ " " ++ " " ++ (print_struct_varlist names) ++ ";"

---------------------------------------------------------------
-- Wire
data WireType = WireSimple | WireTri
    deriving(Eq,Ord,Show,Read)

instance ASTPrinter WireType where
    print_struct WireTri = "tri"
    print_struct WireSimple = "wire"

data Wire = 
    Wire WireType Range [String]
    deriving(Eq,Ord,Show,Read)

instance ASTPrinter Wire where
    print_struct (Wire t r names) = 
        print_struct t ++ " " ++ print_struct r ++ " " ++ print_struct_varlist names ++ ";"

---------------------------------------------------------------
-- Parameter
data Parameter = 
    Parameter String Expr
    deriving(Eq,Ord,Show,Read)

instance ASTPrinter Parameter where
    print_struct (Parameter name value) = 
        "parameter " ++ name ++ " = " ++ print_struct value ++ ";"


---------------------------------------------------------------
-- Stm's

-- q[0] | .port(q[0])
{-
data FormalExpr = 
    FormalSimple Expr |
    FormalStrict String Expr
    deriving(Eq,Ord,Show,Read)
-}

-- (.q(x),a.(c))
data SubmoduleFlist = 
    SubmoduleFlist [Expr] |
    SubmoduleFlistStrict [(String,Expr)]
    deriving(Eq,Ord,Show,Read)

instance ASTPrinter SubmoduleFlist where
    print_struct (SubmoduleFlist exs) =  print_struct_varlist $ map print_struct exs
    print_struct (SubmoduleFlistStrict pairs) = print_struct_varlist $ map print_pair pairs
        where
        print_pair (s,expr) = "." ++ s ++ "(" ++ (print_struct expr) ++ ")"
    

data Stm = 
    -- assign q = a
    Assign Expr Expr |
--    CAssign PortName Expr |
--    Alias PortName PortName |
    -- type [id] (.port1(x),.port2(y))
    -- type id formal_list
    Submodule String (Maybe String) SubmoduleFlist |

    Defparam String String Expr

    deriving(Eq,Ord,Show,Read)

instance ASTPrinter Stm where
    print_struct (Assign e1 e2) = 
        "assign " ++ print_struct e1 ++ " = " ++ (print_struct e2) ++ ";"
        
    print_struct (Submodule t n fl) = 
        t ++ " " ++ (get_name n) ++ " (" ++ print_struct fl ++ ");"
        where
        get_name (Just x) = x
        get_name Nothing  = ""

    print_struct (Defparam v a ex) = 
        "defparam " ++ v ++ "." ++ a ++ " = " ++ (print_struct ex) ++ ";"

    -- print_struct  a = show a

--------------------------------------------------------------
-- Expr


--------------------------------------------------------------
{-
test :: IO ()
test = do
    print test_ast
    return ()
    where
    test_ast = Module "test" ["a","b","q"] mstm'
        where
        mstm' = ModuleStms ports []       []     []
--        mstm = ModuleStms ports wires [param] stms
        ports = [PortDecl Input EmptyPortType (Range (DecDigit "0") (DecDigit "7")) ["a","b"]
                ,PortDecl Output EmptyPortType (Range (DecDigit "0") (DecDigit "15")) ["q"]]
        param = ParameterStr "p1" "1\'b0"
        wires = []
        stms = [Assign (LValueSimple (Var "q" EmptyRange)) (LValuePartial [ 
                    LValueSimple (Var "a" (Range (DecDigit "0") (DecDigit "7"))),
                    LValueSimple (Var "b" (Range (DecDigit "0") (DecDigit "7")) )] )]
-}


