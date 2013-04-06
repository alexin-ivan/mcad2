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
    to_verilog [] = ""
    to_verilog xs = (concat $ intersperse "#" $ map to_verilog xs)

instance ToVerilog S.InstanceName where
    to_verilog (S.InstanceName s) = s
    to_verilog (S.InstanceIID s i) = s ++ "[" ++ (show i) ++ "]"

instance ToVerilog NodeFormalList where
    to_verilog (NodeFormalList flist) = let body = concat $ intersperse "," $ map go $ Map.toList flist
            in printf "(%s)" body
        where
        go (wire,port) = printf ".%s (%s )" sport swire
            where
            swire = to_verilog $ S3.wire_arg wire
            sport = to_verilog $ conv_arg $ S3.port_arg port
            conv_arg (S3.Arg _ lid) = S3.Arg [] lid
            {-conv_arg = id-}

instance ToVerilog S3.Value where
    to_verilog (S3.CValue b)   = S.ast_show b
    to_verilog (S3.SValue arg) = to_verilog arg

instance ToVerilog S3.Arg where
    to_verilog (S3.Arg mid sid) | mid == [] = printf "\\%s" ssid
                                | otherwise = printf "\\%s#%s" smid ssid
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
        show_prefix = printf "%s'b" (show $ length bs)
        show_bit b = c b
            where
            c S.H = "1"
            c S.L = "0"
            c S.X = "x"
            c S.Z = "z"

instance ToVerilog S.ModuleType where
    to_verilog = S.moduletype_name

instance ToVerilog Node where
    to_verilog node | ntype == "alias" = print_alias
                    | ntype == "mux"   = print_mux
                    | otherwise = printf  "%s \\%s %s;\n%s\n" ntype nid flist props
        where
        print_mux :: String
        print_mux = printf "assign %s = %s ? %s : %s ;\n" varL varS varD1 varD0
            where
            varL =  findBy "o"
            varS =  findBy "sel"
            varD0 = findBy "d0"
            varD1 = findBy "d1"
            findBy :: String -> String
            findBy pname = to_verilog $ S3.wire_arg $ case find (pred pname) flist' of
                Nothing -> error $ printf "Assertion: can't find wire for port %s in: %s " (show pname) (show flist')
                Just x -> fst x
            
            pred pname (_,S3.Port (S3.Arg _ (S3.ID n Nothing)) d) = pname == n
            pred _ _ = False
            flist' =  Map.toList $ unNodeFormalList $ node_flist node

        print_alias = printf "assign %s = %s ;\n" varL varR
            where
            varL = to_verilog $ S3.wire_arg $ findByPort "o"
            varR = to_verilog $ S3.wire_arg $ findByPort "i"
            findByPort :: String -> S3.Wire
            findByPort pname = case find (pred pname) flist' of
                Nothing -> error "Assertion: can't find wire for port"
                Just x -> fst x
            
            pred pname (_,S3.Port (S3.Arg _ (S3.ID n Nothing)) d) = pname == n
            pred _ _ = False
            flist' =  Map.toList $ unNodeFormalList $ node_flist node

        removeTopNameFromNodeid = reverse . tail . reverse

        nid   = to_verilog $ removeTopNameFromNodeid $ node_id node
        ntype = to_verilog $ node_type node
        props = concat $ intersperse "\n" $ map print_param $ Map.toList $ S.unLocalParameters $ node_props node
        flist = to_verilog $ node_flist node
        print_param (pid,val) = printf "defparam \\%s .%s = %s;" nid pid sval
            where sval = to_verilog $ Constant val

instance ToVerilog EdgeMap where
    to_verilog = unlines . (map go) . Map.keys
        where
        go :: [S3.Wire] -> String
        go wire = printf "%s %s ;" wtype wid 
            where
            main_wire = last wire
            wid = case S3.wire_arg main_wire of
                (S3.SValue ax) -> to_verilog $ conv_arg ax
                (S3.CValue b ) -> to_verilog $ Constant [b]
            wtype = to_verilog $ S3.wire_type main_wire
            {-conv_arg (S3.Arg _ lid) = S3.Arg [] lid-}
            conv_arg = id

instance ToVerilog AST.WireType where
    to_verilog AST.WireSimple = "wire"
    to_verilog AST.WireTri = "tri"

instance ToVerilog Graph where
    to_verilog graph = printf "module %s(%s);\n%s\n%s\n%s\nendmodule\n" name portdefs portdecls wires nodes
        where
        top = case filter f $ graph_nodes graph of
            [] -> error "Assertion: top module is not defined"
            [x] -> x
            xs -> error "Assertion: multiple top modules"
            where f = S.moduletype_istop . node_type
        name = S.moduletype_name $ node_type top
        
        ports = Map.elems $ unNodeFormalList $ node_flist top

        portdefs = concat $ intersperse "," $ map show_port_name ports
        portdecls = unlines $ map show_port_decl ports

        nodes = concat $ map to_verilog $ filter (not . pred_isBlackBox) $ filter pred_isPrimitive $ graph_nodes graph
        wires = to_verilog $ Map.filterWithKey (\k e -> not $ is_port' k e) $ graph_edges graph
            where
            is_port' _ e = any (`elem` ports) e

            is_port [w] = case S3.wire_arg w of
                (S3.SValue arg) -> (S3.arg_mid arg) == []
                _               -> False
            is_port ws  = False


        pred_isPrimitive = S.moduletype_isprimitive . node_type
        pred_isTop = S.moduletype_istop . node_type
        pred_isBlackBox x =  (pred_isPrimitive x) && (pred_isTop x)

        show_port_name port = (\x -> x ++ " ") $ to_verilog $ (S3.port_arg port) {S3.arg_mid = []}

        show_port_decl port = printf "%s %s ;" d (show_port_name port)
            where
            d = S.ast_show $ S3.port_dir port

do_test_hg_to_v ::  [AST.Module] -> String
do_test_hg_to_v ast = case go of
        Left s -> s
        Right x -> x
    where
    go = do
        asts <- S3.build_gtrees ast
        let graphs = map build asts
        return $ unlines $ map to_verilog graphs

