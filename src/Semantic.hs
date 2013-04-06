{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveFunctor #-}
--
-- Author: Ivan
-- File: Semantic.hs
-- Data: 2013-03-16


module Semantic where

import AST(Direction(..),WireType)
import qualified AST as A

import Data.Map(Map)
import qualified Data.Map as Map

import Data.Tree
import Data.List
import Data.Char(ord,toLower,toUpper)
import Data.Maybe(fromJust)

import Control.Applicative
import Control.Monad.Writer
import Control.Monad.State

import Debug.Trace(trace)

showtrace x = trace (show x) x


class (Eq a, Ord a, Show a, Monoid a) => Path a where
    


class ASTShow a where
    ast_show :: a -> String

data Bit = H | L | X | Z
    deriving(Eq,Ord,Show,Read,Enum)

instance ASTShow Bit where
    ast_show b = "1b'" ++ (c b)
        where
        c H = "1"
        c L = "0"
        c X = "x"
        c Z = "z"

instance ASTShow Direction where
    ast_show Input = "input"
    ast_show Inout = "inout"
    ast_show Output = "output"

enum_bits = [H .. Z]

type Result a = Either String a

data PortDecl = PortDecl Direction Variable
    deriving(Eq,Ord,Show,Read)
data WireDecl = WireDecl WireType Variable
    deriving(Eq,Ord,Show,Read)

data VariableT id = VariableT {
    variable_name :: id,
    variable_range :: [Int] }
    deriving(Eq,Ord,Show,Read,Functor)

data PortDeclT id  = PortDeclT {
    portdeclT_dir :: Direction,
    portdeclT_var :: (VariableT id) }
    deriving(Eq,Ord,Show,Read,Functor)

data WireDeclT id = WireDeclT {
    wiredeclT_type :: WireType,
    wiredeclT_var  ::  (VariableT id) }
    deriving(Eq,Ord,Show,Read,Functor)

data ArgT id = 
    ArgVarT   id (Maybe Int) |
    ArgConstT Bit
    deriving(Eq,Ord,Show,Read,Functor)

make_variable name r = VariableT name r

instance ASTShow PortDecl where
    ast_show (PortDecl d var) = dir ++ " " ++ (ast_show var) ++ ";"
        where
        dir = A.print_struct d

instance ASTShow WireDecl where
    ast_show (WireDecl t var) = wtype ++ " " ++ (ast_show var) ++ ";"
        where
        wtype = A.print_struct t
{-
data Variable = Variable String [Int]
    deriving(Eq,Ord,Show,Read)
-}

type Variable = VariableT String

instance ASTShow Variable where
    ast_show (VariableT name []   ) = name
    ast_show (VariableT name range) = concat $ intersperse "," $ 
        map (\x ->  name ++ "~" ++ (show x)) range

type VarList arg = [arg]

-- | Var -> Port
type FormalList arg = [(VarList arg, String)]

data Sym = 
    SymVar String [Int] |
    SymConst [Bit]
    deriving(Eq,Ord,Show,Read)

data Arg = 
    ArgVar String (Maybe Int) |
    ArgConst Bit
    deriving(Eq,Ord,Show,Read)

instance ASTShow Arg where
    ast_show (ArgVar name Nothing) = name
    ast_show (ArgVar name (Just i)) = name ++ "~" ++ (show i)
    ast_show (ArgConst b) = ast_show b

newtype FormalMap arg = FormalMap (Map arg PortDecl)
    deriving(Eq,Ord,Show,Read)

newtype FormalMapT id = FormalMapT { unFormalMapT :: (Map (ArgT id) (PortDeclT id)) }
    deriving(Eq,Ord,Show,Read)

instance (ASTShow arg) => ASTShow (FormalMap arg) where
    ast_show (FormalMap fm) = concat $ intersperse "," $ map go $ Map.toList fm
        where
        go (a,pd@(PortDecl d v)) = "." ++ (ast_show $ portDeclToArg pd ) ++ "(" ++ (ast_show a) ++ ")"

empry_formal_map = FormalMap Map.empty

data Parameter pid v = Parameter pid v
    deriving(Eq,Ord,Show,Read,Functor)

type CEnv = Map String [Bit]
type SEnv = Map String Sym

type Env = Map String [Arg]

newtype LocalParameters = LocalParameters { unLocalParameters :: CEnv }
    deriving(Eq,Ord,Show,Read)

instance ASTShow LocalParameters where
    ast_show (LocalParameters cenv) = concat $ map go $ Map.toList cenv
        where
        go (n,v) = "parameter " ++ n ++ (show v) ++ ";"


empry_local_parameters = LocalParameters Map.empty

{- {{{
data Submodule = 
    Submodule Module String FormalList [Parameter]
    deriving(Eq,Ord,Show,Read)

data Module = 
    Module String [PortDecl] [WireDecl] [Parameter] [Submodule]
    deriving(Eq,Ord,Show,Read)
-}

{-
data Submodule = 
    Submodule Module String FormalList [Submodule]
    deriving(Eq,Ord,Show,Read)
}}} -}


data InstanceName = 
    InstanceName String |
    InstanceIID String Int |
    InstanceUnnamed
    deriving(Eq,Ord,Show,Read)

instance_name_to_maybe (InstanceName x) = Just x
instance_name_to_maybe (InstanceIID s i) = Just $ s ++ "~" ++ (show i)
instance_name_to_maybe InstanceUnnamed = Nothing

type ModuleType   = String

data Module = 
    Module ModuleType [PortDecl] [WireDecl] LocalParameters InstanceName (FormalMap Arg)
    deriving(Eq,Ord,Show,Read)

data ModuleT id = 
    ModuleT {
    moduleT_type   :: ModuleType,
    moduleT_ports  :: [PortDeclT id],
    moduleT_wires  :: [WireDeclT id],
    moduleT_params :: LocalParameters,
    moduleT_id     :: id,
    moduleT_flist  :: (FormalMapT id) }
    deriving(Eq,Ord,Show,Read)


instance ASTShow Module where
    ast_show (Module t ps ws pr name' fl) = concat $
        [name ++ " " ++ t ++ " " ++ (ast_show fl) ++ ";"] ++
        (map ast_show ps) ++
        (map ast_show ws) ++
        [ast_show pr] ++
        ["endmodule"]
        where
        name = case instance_name_to_maybe name' of
            Nothing -> "module"
            Just x  -> x

--instance Show Module where
--    show = ast_show
-- show_fmap flist = show $ Map.fromList flist

{-
instance Show Module where
    show (Module t ps ws env Nothing     flist) = t ++ " anonimus " ++ (show ps) ++ " " ++ (show ws) ++ " " ++ (show env) ++ " " ++ (show flist)
    show (Module t ps ws env (Just name) flist) = t ++ " " ++ name  ++ " " ++ (show ps) ++ " " ++ (show ws) ++ " " ++ (show env) ++ " " ++ (show flist)
-}

type AST = Tree Module

type ASTT id = Tree (ModuleT id)

-- {{{Convert to T

convertTreeToTreeT (Node m ms) = convertTnode (\x -> [x]) $ Node (expand_top m) ms

convertTnode ::  Ord b => (String -> b) -> Tree Module -> Tree (ModuleT b)
convertTnode f (Node (Module t pd wd pr n fm) ts) = Node (ModuleT t pdT wdT pr nT fmT) (map (convertTnode f) ts)
    where
    pdT = map (convertTportDecl f) pd
    wdT = map (convertTwireDecl f) wd
    nT  =  case instance_name_to_maybe n of
        Nothing -> error $ "Assertion error: instance name is not defined in function `convertTnode`"
        Just x -> f x
    fmT = convertTformalMap f fm



convertTportDecl f (PortDecl d v) = PortDeclT d (fmap f v)
convertTwireDecl f (WireDecl t v) = WireDeclT t (fmap f v)

convertTformalMap :: Ord b => (String -> b) -> FormalMap Arg -> FormalMapT b
convertTformalMap f (FormalMap fm) = FormalMapT $ Map.mapKeys convarg $ Map.map (convertTportDecl f) fm
    where
    convarg (ArgVar n r) = ArgVarT (f n) r
    convarg (ArgConst b) = ArgConstT b

-- }}}

-- {{{Expand Top

expand_top (Module t pd wd pr mn (FormalMap fm)) = Module t pd wd pr mn $ FormalMap newfm
    where
    newfm = Map.fromList $ concatMap go pd
        where
        go p@(PortDecl d (VariableT n [])) = [(ArgVar n Nothing,p)]
        go p@(PortDecl d (VariableT n r)) = map gen r
            where
            gen i = (ArgVar n $ Just i,p)

-- }}}

make_ports :: CEnv -> [A.PortDecl] -> [String] -> Result [PortDecl]
make_ports env port_decls port_defs = liftM concat $ mapM go port_decls
    where
    go (A.PortDecl dir r names) = mapM f names
        where
        f n = do
            if (not (n `elem` port_defs)) then Left $ "Lost port: " ++ n ++ " ;"
             else do
              var   <- make_range_decl env r n
              return $ PortDecl dir var 
 
make_wires env wires_decl = liftM concat $ mapM go wires_decl
    where
    go (A.Wire t r names) = mapM f names
        where
        f n = liftM (\v -> WireDecl t v )  $ make_range_decl env r n


make_range_decl :: CEnv -> A.Range -> String -> Result Variable
make_range_decl env r n = do
        range <- const_cast_range env r
        return $ make_variable n $ case range of
            [] -> []
            [x] -> [0..x]
            xs -> xs

{-
make_params :: CEnv -> [A.Parameter] -> Result CEnv
make_params redefs selfs = Map.traverseWithKey (\k v -> eval_constant_with_default redefs k v) selfsMap
    where
    aparam_to_pair (A.Parameter n e) = (n,e)
    selfsMap  = Map.fromList $ map aparam_to_pair selfs
-}

make_params :: CEnv -> [A.Parameter] -> Result CEnv
make_params redefs selfs = foldM go redefs $ map aparam_to_pair $ selfs
    where
    go env (k,e) =  liftM (\v -> Map.insert k v env) $ eval_constant_with_default env k e
    aparam_to_pair (A.Parameter n e) = (n,e)

expand_flist :: (Show a, Ord a) => (FormalList a) -> [PortDecl] -> Result (FormalMap a)
expand_flist flist port_decls = case mapM go flist of
        Nothing -> Left $ "Can't make bindings in flist:" ++ (show flist) ++ " ;"
        Just xs -> Right $ FormalMap $ Map.fromList $ concat xs
    where
    ports_with_names =  map (\p@(PortDecl _ v ) -> (variable_name v,p) ) port_decls

    go :: (VarList a,String) -> Maybe [(a,PortDecl)]
    go (v,p) = do
            port <- lookup p ports_with_names
            let port_list = expand_port_decl port
            return $ zip v port_list
        where
        expand_port_decl p@(PortDecl d (VariableT n [])) = [p]
        expand_port_decl (PortDecl d (VariableT n r))  = map (\x -> PortDecl d (make_variable n [x])) r



-- {{{ Evals
const_cast_range :: CEnv -> A.Range -> Result [Int]

const_cast_range env (A.Range b_expr e_expr) =
        (pure mk_range) <*> (eval' b_expr) <*> (eval' e_expr)
    where
    mk_range b e = if b < e then [b..e] else reverse [e..b]
    eval' e = do
        bits <- eval_constant env e
        return $ bits_to_int bits
{-
        range <- const_cast_range env r
        return $ apply_range value range
-}

const_cast_range env (A.Index i_expr) = pure (\x -> [bits_to_int x]) <*> (eval_constant env i_expr)
const_cast_range env (A.EmptyRange) = pure []

findValue name env = 
    case Map.lookup name env of
        Nothing -> Left $ "Not find variable `" ++ name ++ "` in env: " ++ (show env) ++ " ;"
        Just x  -> Right x

senv_to_cenv :: SEnv -> CEnv
senv_to_cenv = error $ "senv_to_cenv is undefined"

eval_arg :: SEnv -> A.Expr -> Result (VarList Arg)
eval_arg env econst@(A.E_Constant c) = liftM (map ArgConst) $ eval_constant cenv econst
    where
    cenv = senv_to_cenv env

eval_arg env (A.E_Union xs) = liftM (concat) $ mapM (eval_arg env) xs

eval_arg env (A.E_Var name r) =
    case findCValue name env of
        Right value -> do -- const param
            range <- const_cast_range cenv r
            const_value <- const_cast name value
            return $ map ArgConst $ apply_range_c const_value range
        Left _ -> do  -- wire
            range' <- const_cast_range cenv r
            (SymVar _ fullr) <- findValue name env
            let range = apply_partial_range fullr range'
            return $ map (ArgVar name) $ apply_range_v range
    where

    apply_partial_range self_r []        = self_r
    apply_partial_range self_r partial_r = partial_r
        where
        app i = self_r!!i

    
    findCValue name env = case findValue name env of
        Right c@(SymConst x) -> Right c
        _                  -> Left ""

    cenv = senv_to_cenv env
    const_cast name (SymConst bits) = Right $ bits
    const_cast name (SymVar   _ _ )   = Left  $ "Const cast: " ++ (show name)
    
    apply_range_v []    = [Nothing]
    apply_range_v range = map Just range

    apply_range_c :: [a] -> [Int] -> [a]
    apply_range_c bits [] = bits
    apply_range_c bits range =  map (\i -> bits!!i) range

eval_arg env (A.E_BitOp e) = Left $ "BitOp NotImplement"
eval_arg env (A.E_ConstantRange c r) = do
    range <- const_cast_range cenv r
    bits  <- eval_aconstant c
    return $ map (ArgConst ) $ apply_range_c bits range
    where
    cenv = senv_to_cenv env
    apply_range_c :: [a] -> [Int] -> [a]
    apply_range_c bits [] = bits
    apply_range_c bits range =  map (\i -> bits!!i) range

eval_constant_with_default env name c =
    case findValue name env of
        Right x -> return x
        Left _  -> eval_constant env c

eval_constant :: CEnv -> A.Expr -> Result [Bit]
eval_constant env (A.E_Constant c) = eval_aconstant c
eval_constant env (A.E_Var name r) = do
        value <- findValue name env
--        let constvalue = map (\(ArgConst x) -> x) value
--        const_value <- const_cast name value
        range <- const_cast_range env r
        return $ apply_range value range
    where
    const_cast name (SymConst bits) = Right $ bits
    const_cast name (SymVar   _  _)   = Left  $ "Const cast: " ++ (show name)
    apply_range :: [Bit] -> [Int] -> [Bit]
    apply_range bits [] = bits
    apply_range bits r =  map (\i -> bits!!i) r
    

eval_constant env (A.E_ConstantRange c r) = do
    range <- const_cast_range env r
    bits  <- eval_aconstant c
    return $ apply_range_c bits range
    where
    apply_range_c :: [a] -> [Int] -> [a]
    apply_range_c bits [] = bits
    apply_range_c bits range =  map (\i -> bits!!i) range

eval_constant env e               = Left $ "Expr `" ++ (show e) ++ "` is not constant"

eval_aconstant :: A.Const -> Result [Bit]
eval_aconstant (A.C_String s) = (Right . concat . (map int_to_bits) . (map ord)) s
eval_aconstant (A.C_Digit  d) = digit_to_bits d

test_range = A.Index e
    where
    e = A.E_Constant c
    c = A.C_Digit d
    d = A.DecDigit "10"

-- }}}
-- digits {{{
digit_to_bits (A.DecDigit d) = Right $ int_to_bits (read d)
digit_to_bits (A.BinDigit d) = read_bin_digit d
digit_to_bits (A.BinIxDigit d) = read_bin_digit d


zipWithDefault ::  (a -> a -> b) -> a -> [a] -> [a] -> [b]
zipWithDefault f def [] []         = []
zipWithDefault f def [] (y:ys)     = (f def y) : zipWithDefault f def [] ys
zipWithDefault f def (x:xs) []     = (f x def) : zipWithDefault f def xs []
zipWithDefault f def (x:xs) (y:ys) = (f x y)   : zipWithDefault f def xs ys

read_bin_digit :: String -> Result [Bit]
read_bin_digit s = do
    let len = length value
    let default_value = L
    let prefix = replicate (range - len) default_value
    result <- mapM (\c -> to_either c $ lookup c table) value
    return $ (prefix ++ result)
    where
    (range,value) = split_digit s
    to_either _ (Just x) = Right x
    to_either v Nothing  = Left $ "Can't find digit symbol for value '" ++ v : "'"
    table = [('0',L)
        ,('1',H)
        ,('x',X)
        ,('z',Z)]

split_digit :: String -> (Int, String)
split_digit s = (read prefix, value )
    where
    (prefix,val) = span (/= '\'') s
    value = tail $ map toLower $ tail val

--- }}}
-- {{{bits

int_to_bits ::  Int -> [Bit]
int_to_bits value = reverse $ map to_bit $ go value
    where
    to_bit 0 = L
    to_bit 1 = H
    
    go 0 = [0]
    go 1 = [1]
    go v = let i = v `div` 2
               r = v `mod` 2
            in r : go ((v - r) `div` 2)


bit_bool H = Just True
bit_bool L = Just False
bit_bool _ = Nothing

bits_to_int bits = snd $ foldr (\v (p,r) ->  (p*2, r + (to_int v) * p) ) (1,0) bits
    where
    to_int v = case bit_bool v of
                    Just True -> 1
                    Just False -> 0
                    Nothing -> error $ "Undefined int value for `" ++ (show v) ++ "` in Eval.hs"

-- }}}


{-
make_stms :: 
    [PortDecl] -> 
    [WireDecl] -> 

    String     -> 
    CEnv       -> 
    [A.Stm]    -> 
    [A.Module] -> 
    
    [Tree Module]
-}

make_stms ports wires instance_name cenv stms ast = do
        submodules <- mapM (\s -> make_stm s) submodule_stms
        return $ rename submodules
    where

    rename subs = evalState (mapM go subs) indexMap
        where
        go :: Tree Module -> State (Map ModuleType Int) (Tree Module)
        go (Node (Module t pd wd lpr InstanceUnnamed fm) rest) = do
            i <- gets $ \m -> fromJust $ Map.lookup t m
            modify $ Map.update (\x -> Just $ x + 1) t
            return $ Node (Module t pd wd lpr (InstanceIID t i) fm ) rest
        
        go x  = return x

        indexMap = Map.fromList $ map f subs
            where
            f (Node (Module t _ _ _ _ _) _) = (t,0)



    env :: SEnv
    env = Map.union cenv' (Map.fromList $ vars_to_argmap $ ports_var ++ wires_var)
        where
        cenv' = Map.map SymConst cenv
        ports_var = map go ports
            where go (PortDecl _ var) = var
        wires_var = map go wires
            where go (WireDecl _ var) = var
        vars_to_argmap xs = map go xs
            where 
            go var = (variable_name var,SymVar (variable_name var) range)
                where
                name = variable_name var
                range = variable_range var

    findModule name = case filter (\m -> A.module_name m == name) ast of
        [] -> Left $ "Can't find module " ++ name
        [x] -> Right $ x
        xs -> Left $ "Multiple modules " ++ name
    
    filterDefs Nothing     = []
    filterDefs (Just name) = filter f stms
        where
        f (A.Defparam inst n e) = inst == name
        f  _                    = False

    submodule_stms = filter f stms
        where
        f (A.Submodule _ _ _) = True
        f (A.Assign _ _ )     = True
        f _                   = False

    assign_stms = filter f stms
        where
        f (A.Assign _ _ ) = True
        f _               = False

    defparam_to_param (A.Defparam inst n e) = A.Parameter n e
    
    aflist_to_flist port_defs (A.SubmoduleFlist flist) = zipWithM go flist [0..]
        where
        go e i = do
            var_list <- eval_arg env e
            return (var_list,port_name)
            where
            port_name = port_defs!!i

    aflist_to_flist port_defs (A.SubmoduleFlistStrict flist) = mapM go flist
        where
        go (port_name,e) = do
            var_list <- eval_arg env e
            case port_name `elem` port_defs of
                True -> Right (var_list,port_name)
                False -> Left $ "Error in bindings: " ++ (show flist) ++ " ;"
    
    make_stm :: A.Stm -> Result (Tree Module)
    make_stm (A.Assign rvalue lvalue) = do -- Right $ Node (Module [] [] [] Map.empty Nothing Map.empty) []
        r <- make_rvalue
        make_lvalue r lvalue
        where
        make_rvalue = eval_arg env rvalue
        make_lvalue r (A.E_BitOp (A.BE_Select sel a1 a0)) = do
            s <- eval_arg env sel
            d1 <- eval_arg env a1
            d0 <- eval_arg env a0
            m <- make_select r s d1 d0
            return $ Node m []

        -- alias
        make_lvalue  r e@(A.E_Var _ _) = do
            l <- eval_arg env e
            m <- make_alias r l
            return $ Node m []

        -- attach
        make_lvalue  r cr@(A.E_ConstantRange _ _) = Right $ Node (Module "attach" [] [] empry_local_parameters InstanceUnnamed empry_formal_map) []
        make_lvalue  r u@(A.E_Union xs) = do
            l <- eval_arg env u
            m <- make_alias r l
            return  $ Node m []

        -- mux
        make_select varlistR varlistS varlistD1 varlistD0 =
            let select  = PortDecl A.Input  (make_variable "sel" [0..(length varlistS - 1)] )
                d1      = PortDecl A.Input  (make_variable "d1"  [0..(length varlistD1 - 1)])
                d0      = PortDecl A.Input  (make_variable "d0"  [0..(length varlistD0 - 1)])
                outputs = PortDecl A.Output (make_variable "o"   [0..(length varlistR - 1)])
                flist = [(varlistR,"o"),(varlistS,"sel"),(varlistD1,"d1"),(varlistD0,"d0")]
                ports = [select,d1,d0,outputs]
             in  if ((length varlistD0) == (length varlistD1)) && ((length varlistR) == (length varlistD0)) then
                    liftM (\fl -> Module "mux" ports [] empry_local_parameters InstanceUnnamed fl) $ expand_flist flist ports
                 else Left $ "Incomplete bindings in assign: " ++ (show flist)
            

        make_alias :: VarList Arg -> VarList Arg -> Result Module
        make_alias varlistR varlistL =
            let inputs = PortDecl A.Input   (make_variable "i"   [0..(length varlistL - 1)])
                outputs = PortDecl A.Output (make_variable "o" [0..(length varlistR - 1)])
                flist = [(varlistL,"i"),(varlistR,"o")]
                ports = [inputs,outputs]
             in  if (length varlistR) == (length varlistL) then
                    liftM (\fl -> Module "alias" ports [] empry_local_parameters InstanceUnnamed fl) $ expand_flist flist ports
                 else Left $ "Incomplete bindings in assign: " ++ (show flist)

    make_stm (A.Submodule mtype name aflist) = do
        let defparams = filterDefs name
            params'    = map defparam_to_param defparams
            name' = case name of
                        Nothing -> InstanceUnnamed
                        Just x  -> InstanceName x
        params <- make_params cenv params'
        ast_m <- findModule mtype
        flist <- aflist_to_flist (A.module_portdef ast_m) aflist
        m <- make_module params name' flist ast_m ast
        return m

params_to_cenv :: [Parameter String [Bit]] -> CEnv
params_to_cenv p = Map.fromList $ map param_to_pair p
    where
    param_to_pair (Parameter pid v) = (pid,v)

portDeclToArgT (PortDeclT _ (VariableT n r)) = case r of
       [] -> ArgVarT n Nothing
       [x] -> ArgVarT n $ Just x
       xs -> error "Assertion: portDeclToArg"

portDeclToArg (PortDecl _ (VariableT n r)) = case r of
       [] -> ArgVar n Nothing
       [x] -> ArgVar n $ Just x
       xs -> error "Assertion: portDeclToArg"


make_module :: CEnv-> InstanceName -> FormalList Arg -> A.Module-> [A.Module]-> Either String (Tree Module)
make_module def_params instance_name flist m ast = do
        params <- make_params def_params self_params
        ports <- make_ports params port_decls port_defs
        wires <- make_wires params wire_delcs
        formalmap <- expand_flist flist ports
        let subs' = make_stms ports wires instance_name params stms ast -- :: Result [Tree Module]
        subs <- case subs' of
            Left s -> Left $ "Error in module `" ++ show (name,instance_name) ++ "`:" ++ s
            Right x -> Right x
--        let subs = []
        let my = Module name ports wires (LocalParameters params) instance_name formalmap
        return $ Node my subs
    where
    port_decls = A.module_portdecl m
    port_defs  = A.module_portdef  m
    wire_delcs = A.module_wires m
    name       = A.module_name m
    stms       = A.module_stms m
    self_params = A.module_params m



