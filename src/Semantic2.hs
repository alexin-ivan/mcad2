{-# LANGUAGE GADTs #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveFunctor #-}
--
-- Author: Ivan
-- File: Semantic2.hs
-- Data: 2013-03-17


module Semantic2 where

import AST(Direction(..),WireType(..),BitExpr(..))
import qualified AST as A

import Data.Map(Map)
import qualified Data.Map as Map

-- import Semantic(Bit(..))
import Data.Tree
import Data.List
import Data.Char(ord,toLower,toUpper)
import Data.Maybe(fromJust)

-- import Control.Applicative
import Control.Monad.Writer
import Control.Monad.State
import Data.Ix
import Debug.Trace(trace)

showtrace x = trace (show x) x

type Result a = Either String a

data ID = ID String Int -- | CID Bit
    deriving(Eq,Ord,Show,Read)


type LocalID = ID
type GlobalID = [ID]

data Var 
    = GlobalVar [ID]
    | LocalVar ID
    | ConstVar Bit
    deriving(Eq,Ord,Show,Read)


data VarProp = 
    IsPort Direction |
    IsWire WireType
    deriving(Eq,Ord,Show,Read)

data Net = Net Var VarProp
    deriving(Eq,Ord,Show,Read)

-- {{{ Bits
type IxBit = Bool

data NoIxBit = Z | X
    deriving(Eq,Ord,Show,Read)

data Bit = IxBit IxBit | NoIxBit NoIxBit
    deriving(Eq,Ord,Show,Read)


class ToInt a where
    toInt :: a -> Maybe Int

instance ToInt Bool where
    toInt False = Just 0
    toInt True  = Just 1

instance ToInt NoIxBit where
    toInt = const Nothing
-- }}}


class IsLocal a where
    is_local :: a -> Bool

    filter_locals :: [a] -> [a]
    filter_locals = filter is_local

    filter_globals :: [a] -> [a]
    filter_globals = filter (not . is_local)

instance IsLocal [ID] where
    is_local = const False

instance IsLocal ID where
    is_local = const True

instance IsLocal Var where
    is_local = var_is_local
        where
        var_is_local (LocalVar _) = True
        var_is_local _            = False

instance IsLocal Net where
    is_local (Net v _ ) = is_local v

class DebugShow a where
    debug_show :: a -> String

class (Ord k) => IxContainer c k v | c -> v where
    findv :: k -> c -> Result v

instance  (Ord k,DebugShow k) => IxContainer (Map k v) k v where
    findv k c = case Map.lookup k c of
                Nothing -> Left $ "Can't find key: " ++ debug_show k
                Just x  -> Right x
{-
class Eval a r | a -> r where
--    eval :: a -> r
    evalWith :: (IxContainer c k v) => a -> c -> r
-}

class Eval e r | e -> r where
    evalExpr :: (IxContainer c k v) => e -> c -> r

instance Eval e A.Const where
    evalExpr (A.E_Constant c) = const c

{-
data IxBit = L | H
    deriving(Eq,Ord,Show,Read)
-}

{-
data Arg aid = Arg aid (Maybe Int) | Const Bit

data ArgList a b p where
    Port :: (GlobalName aid,GlobalName pid) => pid -> p -> [Arg aid] -> ArgList pid aid p
    Wire :: (GlobalName aid,GlobalName wid) => wid -> p  -> [Arg aid] -> ArgList wid aid p
    Parameter :: (GlobalName pid, GlobalName aid) => aid -> [Arg aid] -> ArgList pid aid ()

instance (GlobalName a) => Show (ArgList a b p) where
    show = toString . get_name

class (Show a) => GlobalName a where
    toString :: a -> String
    toString = show
 
instance GlobalName [String] where
instance GlobalName String where

class ArgConainer c aid | c -> aid where
    get_args :: c -> [Arg aid]

instance ArgConainer (ArgList b a p) a where
    get_args (Port _ _ a) = a
    get_args (Wire _ _ a) = a
    get_args (Parameter _ a) = a

class (GlobalName oid) => GlobalObject c oid | c -> oid where
    get_name :: c -> oid

instance (GlobalName a) => GlobalObject (ArgList a b p) a where
    get_name (Port n _ _ ) = n
    get_name (Wire n _ _ ) = n

class PropObject c p | c -> p where
    get_prop :: c -> p

instance PropObject (ArgList a b p) p where
    get_prop (Port _ d _ ) = d
    get_prop (Wire _ t _ ) = t

test = do
    let a = Arg "a" Nothing
    let b = Const H
    let arg = [a,b]
    let p = Port ["inp"] Input arg
    let w = Wire "w1"   WireTri arg
    print p
    print $ get_prop p
    print w

-}


{- {{{ Old
data Bit = H | L | X | Z
    deriving(Eq,Ord,Show,Read,Enum)

type Result a = Either String a

type VarID = [String]

data Variable = Variable {
    variable_name ::  VarID,
    variable_range :: [Int] }
    deriving(Eq,Ord,Show,Read)

data LocalVariable = LocalVariable String [Int]
    deriving(Eq,Ord,Show,Read)

data PortDecl = PortDecl Direction LocalVariable
    deriving(Eq,Ord,Show,Read)
data WireDecl = WireDecl WireType LocalVariable
    deriving(Eq,Ord,Show,Read)

data Sym = 
    SymVar   { sym_variable :: Variable } |
    SymConst { sym_constant :: [Bit] }
    deriving(Eq,Ord,Show,Read)

data Arg = 
    ArgVar   { 
        arg_var_id :: VarID,
        arg_var_index :: (Maybe Int)  
    } |
    ArgConst { arg_const_bit  :: Bit }
    deriving(Eq,Ord,Show,Read)

type VarList = [Arg]
type FormalList = [(VarList, String)]

newtype FormalMap = FormalMap { getFormalMap :: (Map Arg PortDecl) }
    deriving(Eq,Ord,Show,Read)

empry_formal_map = FormalMap Map.empty


type CEnv = Map String [Bit]
type SEnv = Map String Sym

newtype LocalParameters = LocalParameters { getLocalParamteres :: CEnv }
    deriving(Eq,Ord,Show,Read)

empry_local_parameters = LocalParameters Map.empty

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
    Module {
    module_type   :: ModuleType,
    module_ports  :: [PortDecl],
    module_wires  :: [WireDecl],
    module_params :: LocalParameters,
    module_id     :: [InstanceName],
    module_flist  :: FormalMap }
    deriving(Eq,Ord,Show,Read)

type AST = Tree Module

{- {{{
 
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



drawModule tree' = drawTree $ (fmap show tree)
    where
    tree = convertTnode (\x -> [x]) tree'

-- do_test :: [A.Module] -> Result [PortDecl]
do_test ast = case mapM (\x -> make_module Map.empty ([InstanceName "top"]) [] x ast) ast of
    Left s -> s
    Right t -> unlines $ map drawModule t
}}} -}

make_wires = undefined
make_stms  = undefined

expand_flist = undefined

make_ports :: 
    CEnv ->
    [A.PortDecl] ->
    [String] ->
    Result [PortDecl]

make_ports env port_decls port_defs = liftM concat $ mapM go port_decls
    where
    go (A.PortDecl dir r names) = mapM f names
        where
        f n = do
            if (not (n `elem` port_defs)) then Left $ "Lost port: " ++ n ++ " ;"
             else do
              var   <- make_range_decl env r n
              return $ PortDecl dir var 

    make_range_decl :: CEnv -> A.Range -> String -> Result LocalVariable
    make_range_decl env r n = do
            range <- const_cast_range env r
            return $ LocalVariable n $ case range of
                [] -> []
                [x] -> [0..x]
                xs -> xs

make_params :: CEnv -> [A.Parameter] -> Result CEnv
make_params redefs selfs = foldM go redefs $ map aparam_to_pair $ selfs
    where
    go env (k,e) =  liftM (\v -> Map.insert k v env) $ eval_constant_with_default env k e
    aparam_to_pair (A.Parameter n e) = (n,e)


-- {{{ make_module :: CEnv-> [InstanceName] -> FormalList Arg -> A.Module-> [A.Module]-> Result AST
make_module def_params instance_name flist m ast = do
    params <- make_params def_params self_params
    ports <- make_ports params port_decls port_defs []
    return ports
{-
        params <- make_params def_params self_params
        ports <- make_ports params port_decls port_defs
        wires <- make_wires params wire_delcs
        formalmap <- expand_flist flist ports
        let subs' = make_stms ports wires instance_name params stms ast -- :: Result [Tree Module]
        subs <- case subs' of
            Left s -> Left $ "Error in module `" ++ show (name,instance_name) ++ "`:" ++ s
            Right x -> Right x
        let my = Module name ports wires (LocalParameters params) instance_name formalmap
        return $ Node my subs
-}
    where
    port_decls = A.module_portdecl m
    port_defs  = A.module_portdef  m
    wire_delcs = A.module_wires m
    name       = A.module_name m
    stms       = A.module_stms m
    self_params = A.module_params m
-- }}}

-- {{{ Evals

symbol_to_arg (SymConst bits) [] = map ArgConst bits
symbol_to_arg (SymConst bits) r  = map ArgConst $ map (\i -> bits!!i) r

symbol_to_arg (SymVar (Variable varid r))  [] = map (\i -> ArgVar varid $ Just i ) r
symbol_to_arg (SymVar (Variable varid vr)) r  = map (\i -> ArgVar varid $ Just (vr!!i)) r

findValue name env = 
    case Map.lookup name env of
        Nothing -> Left $ "Not find variable `" ++ name ++ "` in env: " ++ (show env) ++ " ;"
        Just x  -> Right x

-- {{{ Const's
eval_constant_with_default env name c =
    case findValue name env of
        Right x -> return x
        Left _  -> eval_constant env c

eval_constant :: CEnv -> A.Expr -> Result [Bit]
eval_constant env (A.E_Constant c) = eval_aconstant c
eval_constant env (A.E_Var name r) = do
        value <- findValue name env
        range <- const_cast_range env r
        return $ apply_range value range
    where
    apply_range :: [Bit] -> [Int] -> [Bit]
    apply_range bits r = map arg_const_bit $ symbol_to_arg (SymConst bits) r

eval_constant env (A.E_ConstantRange c r) = do
    range <- const_cast_range env r
    bits  <- eval_aconstant c
    return $ apply_range bits range
    where
    apply_range :: [Bit] -> [Int] -> [Bit]
    apply_range bits r = map arg_const_bit $ symbol_to_arg (SymConst bits) r

eval_constant env e               = Left $ "Expr `" ++ (show e) ++ "` is not constant"

eval_aconstant :: A.Const -> Result [Bit]
eval_aconstant (A.C_String s) = (Right . concat . (map int_to_bits) . (map ord)) s
eval_aconstant (A.C_Digit  d) = digit_to_bits d

const_cast_range :: CEnv -> A.Range -> Result [Int]

const_cast_range env (A.Range b_expr e_expr) =
        (pure mk_range) <*> (eval' b_expr) <*> (eval' e_expr)
    where
    mk_range b e = if b < e then [b..e] else reverse [e..b]
    eval' e = do
        bits <- eval_constant env e
        return $ bits_to_int bits

const_cast_range env (A.Index i_expr) = pure (\x -> [bits_to_int x]) <*> (eval_constant env i_expr)
const_cast_range env (A.EmptyRange) = pure []
-- }}}

{-

findValue name env = 
    case Map.lookup name env of
        Nothing -> Left $ "Not find variable `" ++ name ++ "` in env: " ++ (show env) ++ " ;"
        Just x  -> Right x

senv_to_cenv :: SEnv -> CEnv
senv_to_cenv = undefined

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
            range <- case range' of
                [] -> do -- full range
                    (SymVar _ fullr) <- findValue name env
                    return fullr
                xs -> return xs

            return $ map (ArgVar name) $ apply_range_v range

    where
    
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


test_range = A.Index e
    where
    e = A.E_Constant c
    c = A.C_Digit d
    d = A.DecDigit "10"
-}

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


do_test ast = show $ mapM (\x -> make_module Map.empty (InstanceName "top") [] x ast) ast

}}} -}
