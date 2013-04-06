{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
--
-- Author: Ivan
-- File: HGraphToVerilog.hs
-- Data: 2013-04-06


module HGraphToVerilog where


import HGraphSemantic
import qualified Semantic3 as S3
import qualified Semantic as S
import qualified AST as AST

import Data.List
import Text.Printf

import Data.Map(Map)
import qualified Data.Map as Map

class ToVerilog a where
    to_verilog :: a -> String

instance ToVerilog S3.ModuleID where
    to_verilog xs = concat $ intersperse "|" $ map to_verilog xs

instance ToVerilog S.InstanceName where
    to_verilog (S.InstanceName s) = s
    to_verilog (S.InstanceIID s i) = s ++ "[" ++ (show i) ++ "]"

instance ToVerilog NodeFormalList where
    to_verilog (NodeFormalList flist) = let body = concat $ intersperse "," $ map go $ Map.toList flist
            in printf "(%s)" body
        where
        go (wire,port) = printf ".%s(%s)" sport swire
            where
            swire = to_verilog $ S3.wire_arg wire
            sport = to_verilog $ conv_arg $ S3.port_arg port
            conv_arg (S3.Arg _ lid) = S3.Arg [] lid

instance ToVerilog S3.Value where
    to_verilog (S3.CValue b) = S.ast_show b
    to_verilog (S3.SValue arg) = to_verilog arg

instance ToVerilog S3.Arg where
    to_verilog (S3.Arg mid sid) | mid == [] = printf "%s" ssid
                                | otherwise = printf "%s|%s" smid ssid
        where
        smid = (to_verilog mid) 
        ssid = (to_verilog sid)

instance ToVerilog S3.ID where
    to_verilog (S3.ID s Nothing) = s
    to_verilog (S3.ID s (Just i)) = to_verilog $ S.InstanceIID s i

newtype Constant = Constant [S.Bit]

instance ToVerilog Constant where
    to_verilog (Constant bs) = show_prefix ++ (concatMap show_bit bs)
        where
        show_prefix = printf "%sb'" (show $ length bs)
        show_bit b = c b
            where
            c S.H = "1"
            c S.L = "0"
            c S.X = "x"
            c S.Z = "z"

instance ToVerilog Node where
    to_verilog node = printf  "%s %s %s;\n%s\n" ntype nid flist props
        where
        nid   = to_verilog $ node_id node
        ntype = node_type node
        props = concat $ intersperse "\n" $ map print_param $ Map.toList $ S.unLocalParameters $ node_props node
        flist = to_verilog $ node_flist node
        print_param (pid,val) = printf "defparam %s .%s = %s;" nid pid sval
            where sval = to_verilog $ Constant val

instance ToVerilog Graph where
    to_verilog graph = printf "module top;\n%s\nendmodule\n" nodes
        where
        nodes = concat $ map to_verilog $ graph_nodes graph

do_test_hg_to_v ::  [AST.Module] -> String
do_test_hg_to_v ast = case go of
        Left s -> s
        Right x -> x
    where
    go = do
        asts <- S3.build_gtrees ast
        let graphs = map build asts
        return $ unlines $ map to_verilog graphs

