--
-- Author: Ivan
-- File: Semantic3.hs
-- Data: 2013-03-19


module Semantic3 where


import qualified Semantic as S

import AST(Direction(..),WireType(..))
import qualified AST as A

import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Tree as Tree
import Control.Monad
import Control.Monad.Writer
import Data.List
import Debug.Trace


data ID = ID {
    id_name :: String,
    id_index :: Maybe Int }
    deriving(Eq,Ord,Show,Read)

type ModuleID = [S.InstanceName]

data Arg = 
    Arg 
    {   arg_mid :: ModuleID
    ,   arg_id  :: ID 
    }
    deriving(Eq,Ord,Show,Read)

instance S.ASTShow Arg where
    ast_show (Arg mid aid) = "" ++ mids ++ "." ++ aids ++ ""
        where
        mids = concat $ intersperse "~" (map showInstName mid)
        aids = showPair (id_name aid) (id_index aid)
        showInstName (S.InstanceName s)  = showPair s Nothing
        showInstName (S.InstanceIID s i) = showPair s (Just i)
        showPair :: String -> Maybe Int -> String
        showPair s Nothing = s
        showPair s (Just i) = s ++ "[" ++ (show i) ++ "]"


data Value = SValue Arg | CValue S.Bit
    deriving(Eq,Ord,Show,Read)

instance S.ASTShow Value where
    ast_show (SValue arg) = S.ast_show arg
    ast_show (CValue b) = show b

data Port = Port 
    {   port_arg :: Arg
    ,   port_dir :: Direction
    }
    deriving(Eq,Ord,Show,Read)

instance S.ASTShow Port where
    ast_show (Port arg d) = (S.ast_show d) ++ " " ++ (S.ast_show arg)

data Wire = Wire
    { wire_arg :: Value
    , wire_type :: A.WireType
    }
    deriving(Eq,Ord,Read,Show)

instance S.ASTShow Wire where
    ast_show (Wire arg A.WireSimple) = "wire " ++ (S.ast_show arg)
    ast_show (Wire arg t) = "tri " ++ (S.ast_show arg)

type PortDecl = [Port]
type WireDecl = [Wire]

newtype FormalMap = FormalMap { unFormalMap :: Map Value Port }
    deriving(Eq,Ord,Show,Read)

instance (S.ASTShow k,S.ASTShow v) => S.ASTShow (Map k v) where
    ast_show flist = "[" ++ midle ++ "]"
        where
        flist' = Map.toList flist
        midle = concat $ intersperse "," some
        some = map S.ast_show flist'

instance (S.ASTShow a) => S.ASTShow [a] where
    ast_show flist = "[" ++ midle ++ "]"
        where
        midle = concat $ intersperse "," some
        some = map S.ast_show flist
    

instance (S.ASTShow a,S.ASTShow b) => S.ASTShow (a,b) where
    ast_show (a,b) = "(" ++ (S.ast_show a) ++ "," ++ (S.ast_show b) ++ ")"

data Module = 
    Module 
    { module_type :: S.ModuleType
    , module_portdecl :: PortDecl
    , module_wiredecl :: WireDecl
    , module_params :: S.LocalParameters
    , module_id :: ModuleID
    , module_flist :: FormalMap
    }
    deriving(Eq,Ord,Show,Read)

type AST = Tree.Tree Module

convert_portdecl context (S.PortDecl d (S.VariableT vname vrange))
        | vrange == [] = [go Nothing]
        | otherwise    = map (go . Just) vrange
    where
    go ix = Port (Arg context (ID vname ix)) d


convert_wiredecl context (S.WireDecl t (S.VariableT vname vrange)) 
        | vrange == [] = [go Nothing]
        | otherwise    = map (go . Just) vrange        
    where
    go ix = Wire (SValue $ Arg context (ID vname ix)) t


portdecl_to_port context (S.PortDecl d (S.VariableT vname vindex') ) = Port arg d
    where
    vindex = case vindex' of
        [] ->  Nothing -- error $ "Assertion portdecl_to_port [] in context: " ++ (show context)
        [x] -> Just x
        xs -> error $ "Assertion portdecl_to_port " ++ (show xs) ++ " in context: " ++ (show context)
    lid = ID vname vindex
    arg = Arg context lid

arg_to_value _ (S.ArgConst b) = CValue b
arg_to_value context (S.ArgVar n i) = SValue arg
    where
    index = case i of
        Nothing -> 0
        Just x  -> x
    lid = ID n i
    arg = Arg context lid

convert_flist currentContext (S.FormalMap flist) = FormalMap $ Map.fromList $ map go $ Map.toList flist
    where
    top_context = tail currentContext
    conv_port = portdecl_to_port currentContext
    conv_arg  = arg_to_value top_context
    go (arg,port) = (conv_arg arg,conv_port port)

convert_module top_context (S.Module t pd wd lp mid flist) =
        Module t g_pd g_wd lp currentContext g_flist
    where
    currentContext = mid : top_context
    g_pd = concatMap (convert_portdecl currentContext) pd
    g_wd = concatMap (convert_wiredecl currentContext) wd
    g_flist = convert_flist currentContext flist

convert_top ast = conv_top $ convert_tree [] ast
    where
    conv_top (Tree.Node m ms) = Tree.Node m' ms
        where
        m' = m {module_flist = FormalMap new_flist}
        portdecl = module_portdecl m
        new_flist = Map.fromList $ map expand_port portdecl

    expand_port port@(Port arg _) = (SValue arg',port)
        where
        arg' = arg { arg_mid = [] }

convert_tree context (Tree.Node m@(S.Module _ _ _ _ mid _) ms) = 
        Tree.Node newModule $ map (convert_tree newContext) ms
    where
    newContext = mid : context
    newModule = convert_module context m


{-
type SymbolMap = Map (ArgT [String]) (ArgT [String])
-- LocalArg -> GlobalArg


const_bit_args :: SymbolMap
const_bit_args = let s =  map ArgConstT enum_bits
                    in Map.fromList $ zip s s

expand_names :: ModuleT [String] -> [String] -> SymbolMap -> (SymbolMap,[WireDeclT [String]])
expand_names m path env = (Map.union const_bit_args $ Map.union p_env w_env, gwires)
    where
    pd = moduleT_ports m
    wd = moduleT_wires m

    fm = moduleT_flist m

    p_env = Map.fromList $ concatMap map_port $ Map.toList $ unFormalMapT fm
    (w_env,gwires) = let (l,g,w) = unzip3 $ concatMap mk_wire wd
                      in (Map.fromList $ zip l g,w)
    map_port :: (ArgT [String],PortDeclT [String]) -> [(ArgT [String],ArgT [String])]
    map_port (arg,port) = case (variable_range var) of
                        [] -> [f Nothing]
                        xs -> map (\x -> f $ Just x) xs
        where
        var = portdeclT_var port
        lname = variable_name var
        f i = case Map.lookup arg env of
                Nothing -> error $ "Assertion error: expand_names " ++ (show arg) ++ " in " ++ (show env)
                Just x  -> (localArg,x)
            where
            localArg = (ArgVarT lname i)
        
    mk_wire wire = case (variable_range var) of
                    [] -> [f Nothing]
                    xs -> map (f . Just) xs
        where
        var = wiredeclT_var wire
        lname = variable_name var
        gname = (lname ++ path)
        gvar Nothing  = var {variable_name  = gname, variable_range = [] }
        gvar (Just i) = var { variable_name = gname, variable_range = [i]  }
        f i = (ArgVarT lname i,ArgVarT gname i, wire { wiredeclT_var = gvar i })

        {-
        f' [pm] = case Map.lookup pm env of
                    Nothing -> error $ "Assertion error: expand_names " ++ pm
                    Just x  -> (pm,x)
        -}
--    mk_wire :: WireDeclT [String] -> (String,[String])
--    mk_wire = (\[wn] -> (wn,wn:path) ) . variable_name . wiredeclT_var

inv_map m = Map.fromList $ map (\(a,b) -> (b,a)) $ Map.toList m

globalize_flist :: SymbolMap -> FormalMapT [String] -> FormalMapT [String]
globalize_flist env (FormalMapT fm) = FormalMapT $ Map.fromList $ map go $ Map.toList fm
    where
    go (garg,larg') = case Map.lookup (portDeclToArgT larg') env of
                Nothing -> error $ "Assertion error: globalize_flist " ++ (show (garg,larg')) ++ " " ++ (show env)
                Just x  -> (x,larg')
    
build path genv (Node m []) = do
    let fm = moduleT_flist m
    let (lenv,wires) = expand_names m  path genv
    let gfm = globalize_flist lenv fm
    let gname = (head $ moduleT_id m) : path
    tell wires
    return [ m { moduleT_flist = gfm, moduleT_id = gname } ]


build path genv (Node m ms) = do
        tell wires
        subs <- mapM ((build newpath lenv) ) ms
        return $ concat subs
    where
    newpath = (head $ moduleT_id m) : path
    (lenv,wires) = expand_names m path genv

build_top node@(Node m ms) =
        let (gsubs,gwires) = runWriter $ build [] new_ps $ node {rootLabel =  m { moduleT_flist = new_fm }}
            in (gsubs,gwires,gports)
    where
    pd = moduleT_ports m
    module_name = moduleT_id m

    gports = Map.elems $ unFormalMapT new_fm
    new_fm = FormalMapT $ Map.fromList $ concatMap go pd
        where
        go port = case variable_range var of
                [] -> [f Nothing]
                xs -> map (f . Just) xs
            where
            var = portdeclT_var port
            lname = variable_name var
            d = portdeclT_dir port
            f Nothing = (ArgVarT lname Nothing, PortDeclT d $ make_variable lname [])
            f i@(Just x) = (ArgVarT lname i, PortDeclT d $ make_variable lname [x])

    new_ps = Map.fromList $ concatMap go pd
        where
        go port = case variable_range var of
                    [] -> [f Nothing]
                    xs -> map (f . Just) xs
            where
            var = portdeclT_var port
            lname = variable_name var
            gname = lname ++ module_name
            f i = (ArgVarT lname i,ArgVarT gname i)

showpath :: [String] -> String
showpath = concat . (intersperse "~")

show_bits bits = prefix ++ (map go bits)
    where
    prefix =  (show $ length bits) ++ "'b"
    go H = '1'
    go L = '0'
    go X = 'x'
    go Z = 'z'

showModuleT :: ModuleT [String] -> String
showModuleT (ModuleT t pd wd (LocalParameters lp) mid fm) =  unlines $
    [t ++ " " ++ name ++ "(" ++ flist ++ ")" ++ ";"] ++ (map showParameter $ Map.toList lp)
    where
    showParameter (n,v) =  "defparam " ++ name ++ "." ++  n ++ " = " ++ (show_bits v) ++ ";"
    name :: String
    name = showpath mid
    flist :: String
    flist = ast_show $ FormalMap $ Map.mapKeys argf $ Map.map pdf $ unFormalMapT fm
        where
        argf (ArgConstT b) = ArgConst b
        argf (ArgVarT n i) = ArgVar (showpath n) i
        pdf (PortDeclT d v) = PortDecl d $ fmap showpath v

showGlobalWires wires = unlines $ map (ast_show . go) wires
    where
    go (WireDeclT t v) = WireDecl t $ fmap showpath v

showGlobalPorts ports = unzip $ map (spl . go) ports
    where
    spl p@(PortDecl d v) = (ast_show v,p)
    go (PortDeclT d v) = PortDecl d $ fmap showpath v

drawModule tree' = "module " ++ mname ++ "(" ++ port_names ++ ");\n" ++ port_decl ++ wires_def ++ body ++ "\nendmodule"
    where
    mname = (\(Module t _ _ _ _ _) -> t) $ rootLabel tree'
    wires_def = showGlobalWires wires
    (port_names',port_decls') = showGlobalPorts gports
    port_names = concat $ intersperse "," port_names'
    port_decl = unlines $ map ast_show port_decls'
    body = unlines $ map showModuleT tree
    (tree,wires,gports) =  build_top $ convertTnode (:[]) tree'

-}

-- do_test :: [A.Module] -> Result [PortDecl]
do_test ast = case mapM (\x -> S.make_module Map.empty (S.InstanceName (A.module_name x) ) [] x ast) ast of
    Left s -> s
--    Right t -> unlines $ map (drawTree . fmap S.ast_show) t
    Right t -> unlines $ map show $ map convert_top t


build_gtrees ::  [A.Module] -> S.Result [Tree.Tree Module]
build_gtrees ast = liftM (map convert_top) $ mapM (\x -> S.make_module Map.empty (S.InstanceName (A.module_name x) ) [] x ast) ast


