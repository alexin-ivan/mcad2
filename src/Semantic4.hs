{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
--
-- Author: Ivan
-- File: Semantic4.hs
-- Data: 2013-03-19


module Semantic4 where

import Data.Map(Map)
import qualified Data.Map as Map

import Data.Set(Set)
import qualified Data.Set as Set

import Data.List
import Data.Tree

import Control.Monad
import Control.Applicative hiding(Const)
import Data.Function

import Bits
import AST(Direction(..))
import AST(WireType(..))
import qualified AST as AST
import Debug.Trace

-- {{{REAL
data ID = ID {
    id_name :: String,
    id_index :: Int }
    deriving(Eq,Ord,Show,Read)

data ArgID = ArgID {
        arg_module_id :: [ID],
        arg_local_id :: ID }
    deriving(Eq,Ord,Show,Read)

data Bus = 
        Port { 
            port_dir :: Direction,
            port_arg :: ArgID } |
        Net  {
            net_type :: WireType,
            net_arg  :: ArgID } | 
        Const {
            const_value :: [Bit] }
    deriving(Eq,Ord,Show,Read)

data Wire = Wire {
    wire_id :: ID,
    wire_bus :: Bus }
    deriving(Eq,Ord,Show,Read)

type FormalList = [(Wire,Wire)]

data Module = Module {
    module_name :: ArgID,
    module_open_wires :: [Wire],
    module_close_wires :: [Wire],
    module_formallist :: FormalList
    }
    deriving(Eq,Ord,Show,Read)

type AST = Tree Module

build_wires  :: [AST.Wire] -> FormalList -> Result [Wire]
build_ports  :: [AST.PortDecl] -> [String] -> FormalList -> Result [Wire]
build_params :: [AST.Parameter] -> FormalList -> Result FormalList
build_stms   :: [AST.Stm] -> FormalList -> Result (Forest Module)

build_wires ast_wires flist = undefined

build_stms = undefined

class EvalExpr r where
    eval :: FormalList -> AST.Expr -> Result r

class Container c where
    expand :: c -> String -> [Int] -> Result [Wire]
    expand_all :: c -> String -> [Wire]
    expand_self :: c -> [Wire]

instance EvalExpr Bus where
    eval = undefined

instance EvalExpr [Wire] where
    eval = undefined

instance EvalExpr Int where
    eval = undefined


instance Container Bus where
    expand (Const bits) name range = undefined
    expand_all bus@(Const bits) name = zipWith (const mk) bits [0..]
        where mk i = Wire (ID name i) bus

    expand_self (Const bits) = undefined
    expand_self (Port d argid) = undefined

eval_range :: FormalList -> AST.Range -> Result [Int]
eval_range flist (AST.Range b e) = do
    b' <- eval flist b
    e' <- eval flist e
    return $ ex b' e'
    where
    ex begin end | begin > end = [end..begin]
                 | begin < end = [begin..end]
                 | otherwise   = [begin]

eval_range flist (AST.Index e) = do
    e' <- eval flist e
    return $ [e']

eval_range flist (AST.EmptyRange) = return $ []

build_ports ast_ports_decl port_defs flist = (liftM concat $ mapM build ast_ports_decl) >>= check
    where
    check :: [(String,Bus)] -> Result [Wire]
    check buss = do
        let uniq_names = nub $ map fst buss
        let wires = concatMap expand_self $ map snd buss
        case port_defs \\ uniq_names of
            [] -> Right wires
            xs -> Left $ "Mismatch port defs"
    build :: AST.PortDecl -> Result [(String,Bus)]
    build (AST.PortDecl d r names) = do
            range <- eval_range flist r
            return $ concat $ map (\n -> go n range) names
        where
        go :: String -> [Int] -> [(String,Bus)]
        go name [] =  go name [0]
        go name range = map (\i -> (name,Port d (argid i))) range
            where argid i = ArgID [] (ID name i)


build_params ast_params flist = do
        params <- pure (concatMap  (\(n,b) -> expand_all b n ) ) <*> (mapM build ast_params)
        let params_flist = map (\x -> (x,x)) params
        return $ unionBy id_cmp flist params_flist
    where
    id_cmp = (==) `on` (wire_id . fst)
    build :: AST.Parameter -> Result (String,Bus)
    build (AST.Parameter name e) = liftM (\x -> (name,x)) $ eval flist e



build_module :: FormalList -> AST.Module -> Result AST
build_module flist m = do
        (ports,wires,lflist) <- local_wires
        let m' = Module mid ports wires lflist
        subs <- build_stms stms lflist
        return $ Node m' subs
    where
    mid = ArgID [] (ID (AST.module_name m) 0)

    local_wires = do
        lflist <- build_params params flist
        ps <- build_ports pdecl pdef lflist
        ws <- build_wires wdecl lflist
        return (ps,ws,lflist)

    pdecl = AST.module_portdecl m
    pdef =  AST.module_portdef m
    wdecl = AST.module_wires m
    params = AST.module_params m
    stms = AST.module_stms m

do_test :: [AST.Module] -> String
do_test ast =
    let r = mapM (build_module []) ast in
    case r of
        Left s -> "Error:" ++ s
        Right x -> show x

-- REAL }}}

{-
data VID mid lid = VID mid lid
    deriving(Eq,Ord,Show,Read)


class GetIID i mid lid | mid lid -> i where
    getLocalID' :: i  -> lid
    getModuleID' :: i -> mid

class SetID i1 i2 mid1 mid2 lid1 lid2 | mid1 lid1 -> i1, mid2 lid2 -> i2 where
    setModuleID' :: (lid1 ~ lid2) => i1 -> mid2 -> i2
    setLocalID'  :: (mid1 ~ mid2) => i2 -> lid2 -> i2
    
modifyModuleID iid formallist = liftM (\x -> setModuleID' iid x) mid2
    where
    mid1 = getModuleID' iid
    mid2 = case Map.lookup mid1 formallist of
        Nothing -> Left $ "Can't find value for key " ++ (show mid1) ++ "in map:" ++ (show formallist)
        Just x  -> Right x

type ID id = (Show id,Eq id,Ord id)


instance GetIID (VID mid lid) mid lid where
    getModuleID' (VID mid lid) = mid
    getLocalID'  (VID mid lid) = lid

instance SetID (VID mid1 lid1) (VID mid2 lid2) mid1 mid2 lid1 lid2 where
    setModuleID' var@(VID mid1 lid1) mid2 = VID mid2 $ getLocalID' var
    setLocalID'  var@(VID mid1 lid1) lid2 = VID (getModuleID' var) lid2



test = do
    let formallist = Map.fromList [("x",["x"])]
    print $ modifyModuleID (VID "x" "a") formallist
-}


{-
newtype Const = Const Bit
    deriving(Eq,Ord,Show,Read)
newtype Var i  = Var i
    deriving(Eq,Ord,Show,Read,Functor)

data Port ip iv = Port ip [Var iv]
    deriving(Eq,Ord,Show,Read)

data Value i = 
    CValue Const | Value (Var i)
    deriving(Eq,Ord,Show,Read,Functor)

data Bus bid vid = Bus bid [Value vid]

class Arg a i | a -> i  where
    solve :: a -> Result Bit

    solveWith    :: (Eq i)  => [(i,Bit)] -> a -> Result Bit
    solveWithMap :: (Ord i) => Map i Bit -> a -> Result Bit
    solveWithMap m i = solveWith (Map.toList m) i


convert_error :: Result Bit
convert_error = Left $ "Can't convert to bit"

instance Arg Const () where
    solve (Const x) = Right x
    solveWith = const solve
    solveWithMap = const solve

instance Arg (Var vid) vid where
    solve = const $ Left "Var can't convert to bit"
    solveWith xs v@(Var i) = case lookup i xs of
                            Nothing -> convert_error
                            Just x  -> Right x
    
    solveWithMap xs (Var i) = case Map.lookup i xs of
                                   Nothing -> Left $ "Can't find value"
                                   Just x  -> Right x

instance Arg (Value i) i where
    solve (CValue c) = solve c
    solve (Value v ) = solve v

-}

{-
instance Arg (Bus bid vid) vid where
    solve (Bus bid vars) = 
        case mapM solveVar vars of
           Nothing -> Left $ "Can't solve"
           Just xs -> Right $ head xs
        where
        solveVar c = solve c
-}

{-
data Port = Port Direction String
    deriving(Eq,Ord,Show,Read)
data Wire = Wire WireType String
    deriving(Eq,Ord,Show,Read)

newtype FormalList = FormalList (Map String String)
    deriving(Eq,Ord,Show,Read)


data Module = 
    Module String [Port] [Wire] [Submodule]
    deriving(Eq,Ord,Show,Read)

data Submodule = 
    Submodule String String FormalList LocalParameters
    deriving(Eq,Ord,Show,Read)


extractWires ms = Set.toList $ foldl f Set.empty ms
    where
    f ws m = Set.union (Set.fromList lwd) ws
        where
        fm = S0.unFormalMapT $ S0.moduleT_flist m
        lwd = map S0.ast_show $ Map.keys fm

test :: IO ()
test = do
    return ()

main :: IO ()
main = do
    print "Hello, World!"
    return ()
-}
