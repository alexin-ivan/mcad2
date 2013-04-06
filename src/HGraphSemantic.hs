{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
--
-- Author: Ivan
-- File: HGraphSemantic.hs
-- Data: 2013-04-03


module HGraphSemantic where

import Semantic3

import qualified Semantic as S

import qualified AST as AST
import AST(Direction(..),WireType(..))

import Data.Map(Map)
import qualified Data.Map as Map

import Data.Set(Set)
import qualified Data.Set as Set

import qualified Data.List as List

import Data.Tree(Tree)
import qualified Data.Tree as Tree

import Control.Applicative
import Control.Monad.State

import Debug.Trace

build :: AST -> Graph
build ast = build_tree (addLocalEdges top (Graph edges nodes)) ast
    where
    top = Tree.rootLabel ast
    formalmap = module_flist top
    flist = Map.toList $ unFormalMap $ formalmap
    nodes = []
    edges = Map.fromList $ map go flist
        where
        go (value,port) = ([eid],[port])
            where
            eid = Wire value WireSimple


type Edge = [Port]
type EdgeMap = Map [Wire] Edge
newtype NodeFormalList = NodeFormalList { unNodeFormalList :: Map Wire Port }
    deriving(Eq,Ord,Show,Read)

data Node = Node
    {   node_id    :: ModuleID
    ,   node_type  :: S.ModuleType
    ,   node_props :: S.LocalParameters
    ,   node_flist :: NodeFormalList
    }
    deriving(Eq,Ord,Show,Read)

data Graph = Graph
    {   graph_edges :: EdgeMap
    ,   graph_nodes :: [Node]
    }
    deriving(Eq,Ord,Show,Read)

instance S.ASTShow Graph where
    ast_show (Graph edges ns) = S.ast_show edges


enter_context_edges ::  FormalMap -> EdgeMap -> EdgeMap
enter_context_edges (FormalMap flist) edges = Map.mapKeys go edges
    where
    enter eid port = lid : eid
        where
        lid = Wire (SValue arg) WireSimple
        arg = port_arg port

    go eid = case Map.lookup value flist of
        Nothing -> eid
        Just x  -> enter eid x
        where value = wire_arg $ head $ eid

leave_context_edges ::  FormalMap -> EdgeMap -> EdgeMap
leave_context_edges (FormalMap flist) edges = Map.mapKeys go edges
    where
    go eid = case Map.lookup value flist of
        Nothing -> eid
        Just x  -> tail eid
        where value = wire_arg $ head $ eid

withEdges f g = g {graph_edges = new_edges }
    where new_edges = f $ graph_edges g

enter_context flist g = withEdges (enter_context_edges flist) g
leave_context flist g = withEdges (leave_context_edges flist) g

addLocalEdges m g = withEdges f g
    where
    wires = module_wiredecl m
    f edges = Map.union edges local_edges
    local_edges = Map.fromList $ map (\w -> ([w],[])) wires

graph_union :: [Graph] -> Graph
graph_union gs = Graph new_edges new_nodes
    where
    new_nodes = concat $ map graph_nodes gs
    new_edges = Map.unionsWith uf $ map graph_edges gs
    uf a b = List.union a b

build_tree ::  Graph -> AST -> Graph
build_tree g (Tree.Node m []) =
        g { graph_edges = new_edges, graph_nodes = new_nodes }
    where
    edges = graph_edges g
    nodes = graph_nodes g

    new_nodes = node : nodes
    node = Node mid t lp $ NodeFormalList fl
        where
        mid = module_id m
        lp = module_params m
        t = module_type m
        fl = Map.fromList $ concatMap local_edges $ Map.toList edges
        local_edges :: ([Wire],Edge) -> [(Wire,Port)]
        local_edges (addr,_) = case Map.lookup k flist of
            Nothing -> []
            Just x  -> [(wire,x)]
            where
            wire = last addr
            k = wire_arg $ head addr


        
     
    flist = unFormalMap $ module_flist m
    new_edges :: EdgeMap
    new_edges = Map.mapWithKey go edges
        where
        go :: [Wire] -> Edge -> Edge
        go wire e = case Map.lookup value flist of
            Nothing -> e
            Just x  -> x : e
            where
            value = wire_arg $ head wire

build_tree g (Tree.Node m ms) = graph_union gs
    where
    flist = module_flist m
    enter_context' = enter_context flist
    leave_context' = leave_context flist
    local_g = addLocalEdges m $ enter_context' g
    gs = map (leave_context' . build_tree local_g) ms

{-
type EdgeLabel = Wire

-- LocalID -> Edge
type EdgeMap = Map [Wire] Edge

data Edge = Edge
    {
    ,   edge_cps :: [Port]
    }
    deriving(Eq,Ord,Show,Read)

data Node = Node
    {   node_id    :: ModuleID
    ,   node_type  :: S.ModuleType
    ,   node_props :: NodeProps
    ,   node_flist :: Map EdgeLabel Port
    }
    deriving(Eq,Ord,Show,Read)

type NodeProps = S.LocalParameters

data Graph = Graph
    {   graph_edges :: EdgeMap
    ,   graph_nodes :: [Module]
    }
    deriving(Eq,Ord,Show,Read)

addPortInEdge edge p = edge { edge_cps = new_cps }
    where new_cps = p : (edge_cps edge)

{-
mapEdgeLabelWith :: (EdgeLabel -> EdgeLabel) -> Graph -> Graph
mapEdgeLabelWith f g = g {graph_edges = new_edges}
    where
    new_edges = Map.mapKeys f $ graph_edges g
-}

invert_formalmap :: FormalMap -> Map Port Value
invert_formalmap flist = Map.fromList $ map (\(x,y) -> (y,x) ) $ Map.toList $ unFormalMap flist

build_graph :: Graph -> AST -> Graph
build_graph gg (Tree.Node m []) =  (\x -> trace (show x) x) $
        gg { graph_edges = new_edges, graph_nodes = new_nodes }
    where
    edges = graph_edges gg
    nodes = graph_nodes gg

    new_nodes = nodes
    
--    flist :: Map Port Value
--    flist = Map.fromList $ map (\(x,y) -> (y,x) ) $ Map.toList $ unFormalMap $ module_flist m
    flist = unFormalMap $ module_flist m
    new_edges :: EdgeMap
    new_edges = Map.mapWithKey go edges
        where
        go wire e = case Map.lookup value flist of
            Nothing -> e
            Just x  -> addPortInEdge e x
            where
            value = wire_arg wire

build_graph gg (Tree.Node m ms) = gg

wireReplaceArg (Wire arg t) value = Wire value t

-- EdgeMap
enter_context :: FormalMap -> EdgeMap -> EdgeMap
enter_context formalmap edges = Map.fromList $ map go $ Map.toList edges
    where
    flist = unFormalMap formalmap
    
    enter :: Wire -> Edge -> Port -> (Wire,Edge)
    enter gid e p = (lid,e')
        where
        e' = e {edge_id = gid }
        lid = Wire (SValue (port_arg p)) AST.WireSimple

    go :: (Wire,Edge) -> (Wire,Edge)
    go (w,e) = case Map.lookup value flist of
        Nothing -> (w,e)
        Just port -> enter w e port
        where
        value = wire_arg w

leave_context :: FormalMap -> EdgeMap -> EdgeMap
leave_context formalmap edges = Map.fromList $ map go $ Map.toList edges
    where
    flist = unFormalMap formalmap
    
    leave :: Wire -> Edge -> Port -> (Wire,Edge)
    leave gid e p = (lid,e')
        where
        e' = e {edge_id = gid }
        lid = Wire (SValue (port_arg p)) AST.WireSimple

    go :: (Wire,Edge) -> (Wire,Edge)
    go (w,e) = case Map.lookup value flist of
        Nothing -> (w,e)
        Just port -> enter w e port
        where
        value = wire_arg w
    

{-
enter_context :: FormalMap -> EdgeMap -> EdgeMap
enter_context formalmap edges = Map.fromList $ map go $ Map.toList flist
    where
    flist = unFormalMap formalmap

    go :: (Value,Port) -> (Port,Edge)
    go (value,port) = (port,edge)
        where
        edge = undefined
-}

empty_graph = Graph (Map.empty) []

-- FormalMap ::  Map Wire Port -> FormalMap
-- EdgeMap :: Map Wire [Port] -> EdgeMap
-- Flist -> Wire -> Wire

build ::  AST -> Graph
build ast = build_graph top_graph ast
    where
    g = empty_graph
    top = Tree.rootLabel ast
--    top_graph = g { graph_edges = Map.mapKeys mk_wire $ Map.map (\x -> Edge x [x]) flist }
    top_graph = g { graph_edges = Map.fromList $ map go $ Map.toList flist }
        where
        go :: (Value, Port) -> (Wire,Edge)
        go (value,port) = (wire,edge)
            where
            edge = Edge wire [port]
            wire = Wire value AST.WireSimple
        flist = unFormalMap $ module_flist top
        mk_wire value = Wire value AST.WireSimple
-}
{-
data PortID = PortID {
    portid_name :: String,
    portid_index :: Int }
    deriving(Eq,Ord,Show,Read)

type NodeID = [S.InstanceName]

data EdgeID = EdgeID {
    edgeid_nodeid:: NodeID,
    edgeid_arg :: S.Arg }
    deriving(Eq,Ord,Show,Read)

data Port = Port 
    {   port_id :: PortID
    ,   port_dir :: Direction
    }
    deriving(Eq,Ord,Show,Read)

data CP = CP {
    cp_node :: Node ,
    cp_port :: Port }
    deriving(Eq,Ord,Show,Read)

data Edge = Edge { edge_cps :: [CP] }
    deriving(Eq,Ord,Show,Read)

data Node
    = Node 
        {   node_id :: NodeID
        ,   node_property :: NodeProperty 
        }
    | UniverseNode
    deriving(Eq,Ord,Show,Read)

type NodeProperty = Map String [S.Bit]

type EdgeMap = Map EdgeID Edge

data Graph = 
    Graph
    { graph_public_edges  :: EdgeMap
    , graph_private_edges :: EdgeMap
    , graph_nodes :: [Node]
    } 
    deriving(Eq,Ord,Show,Read)

empty_graph ::  Graph
empty_graph = Graph (Map.empty) (Map.empty) []

empty_edge ::  Edge
empty_edge = Edge []

cant_find_error ::  (Show c, Show k) => k -> c -> String
cant_find_error k container = 
    "Can't find value " ++ (show k) ++
    "in " ++ (show container)

lookupEdge graph eid = 
    case Map.lookup eid ports of
        Just x -> Right x
        Nothing -> case Map.lookup eid wires of
            Just x -> Right x
            Nothing -> Left $ cant_find_error eid graph
    where
    ports = graph_public_edges graph
    wires = graph_private_edges graph


build_top (S.Module t pd wd lp name flist) =  undefined
    where
    open_edges = map (\x -> (x,empty_edge) ) flist


-}    

{-
class (Ord eid) => EdgeMapC c eid nid | c -> eid, c -> nid where
    lookupEdge :: eid -> c -> Maybe (Edge nid)
    unionEdgeMap :: c -> c -> c

instance (Ord eid) => EdgeMapC (EdgeMap eid nid) eid nid where
    lookupEdge = Map.lookup
    unionEdgeMap = Map.union

instance (Ord eid, Eq nid) => EdgeMapC [(eid,Edge nid)] eid nid where
    lookupEdge = lookup
    unionEdgeMap = List.union

class (EdgeMapC c eid nid) => HGraphClass g c eid nid where
    hg_public_edges :: g -> c
    hg_private_edges :: g -> c
    hg_nodes :: g -> [Node nid]

instance (Ord eid) => HGraphClass (Graph eid nid) (EdgeMap eid nid) eid nid where
    hg_public_edges = graph_public_edges
    hg_private_edges = graph_private_edges
    hg_nodes = graph_nodes
-}





{-
data GraphT eid nid pid = 
    Graph {
    graph_publicEdges  :: [Edge nid pid],
    graph_privateEdges :: (EdgeMap eid nid pid),
    graph_nodes        :: [Node nid] }
    deriving(Eq,Ord,Show,Read)

type Graph = GraphT [String] [String] [String]

newtype EdgeMap eid nid pid = EdgeMap { unEdgeMap :: (Map eid (Edge nid pid)) }
    deriving(Eq,Ord,Show,Read)

newtype NodeMap nid = NodeMap (Map nid (Node nid))
    deriving(Eq,Ord,Show,Read)

data ID gid = ID gid LocalID
    deriving(Eq,Ord,Show,Read,Functor)

data LocalID = LocalID String Int
    deriving(Eq,Ord,Show,Read)

data Port pid = Port Direction pid
    deriving(Eq,Ord,Show,Read,Functor)

instance Applicative Port where
    pure x = Port Inout x
    (Port _ f) <*> (Port d x) = Port d (f x)

instance Applicative ID where
    pure x = ID x (LocalID [] 0)
    (ID f _ ) <*> (ID x localid) = ID (f x) localid

instance Monad ID where
    return = pure
    (ID gid lid) >>= f =
        let (ID gid' _ ) = f gid
         in ID gid' lid

idLocalRenameWith ::  (LocalID -> LocalID) -> ID gid -> ID gid
idLocalRenameWith f (ID g l) = ID g (f l)

data CP nid pid = CP {
    cp_node :: (Node nid),
    cp_port :: (Port pid) }
    deriving(Eq,Ord,Show,Read)

data Edge nid pid = EdgeDir {
    edge_source :: CP nid pid ,
    edge_targets :: (Set (CP nid pid ))}
    | EdgeUndir {
    edge_cpoints :: Set (CP nid pid) }
    deriving(Eq,Ord,Show,Read)

data Node nid 
    = Node 
        {   node_id :: ID nid
        ,   node_property :: NodeProperty 
        }
    | UniverseNode 
    | SupplyNode S.Bit
    deriving(Eq,Ord,Show,Read,Functor)

instance Applicative Node where
    pure x = Node (pure x) (Map.empty)
    (Node f _) <*> (Node nid p) = Node (f <*> nid) p
    UniverseNode <*> _ = UniverseNode
    (SupplyNode b) <*> _ = SupplyNode b
    _ <*> (SupplyNode b) = SupplyNode b
    _ <*> UniverseNode   = UniverseNode

type NodeProperty = Map String [S.Bit]

type Context = [String]


-- convert_portdecl :: Context -> S.PortDecl -> [Edge nid (ID Context)]
convert_portdecl c varfmap nodeid flist (S.PortDecl dir var) = 
    let range = (S.variable_range var)
        in map mk_edge  $ map (CP UniverseNode) $ case range of
            [] -> [go 0]
            xs -> map go xs
    where
    mk_edge cp = EdgeUndir (Set.singleton cp)
    
    lname = S.variable_name var
    go i = Port dir (ID c (LocalID lname i))

convert_wiredecl :: Context -> S.WireDecl -> EdgeMap (ID Context) nid pid
convert_wiredecl c (S.WireDecl t var) = let range = S.variable_range var
        in EdgeMap $ Map.fromList $ map mk_edge $ case range of
            [] -> [go 0]
            xs -> map go xs
    where
    mk_edge wid = (wid,EdgeUndir (Set.empty) )
    lname = S.variable_name var
    go i = ID c (LocalID lname i)


type GraphWithContext = GraphT (ID Context) (ID Context) (ID Context)

build_primitive :: Context -> (String -> Edge (ID Context) (ID Context) ) -> S.Module -> GraphWithContext
build_primitive c varfmap (S.Module t pd wd lp name flist) = Graph public_edges private_edges nodes
    where
    public_edges = concatMap (convert_portdecl c varfmap nodeid flist) pd
    private_edges = EdgeMap $ Map.fromList $ concatMap (Map.toList . unEdgeMap . convert_wiredecl c) wd
    nodes = []
    nodeid = undefined


build_graph c (Tree.Node m []) = build_primitive c undefined m
build_graph c mtree = undefined

-}

do_test_hg ::  [AST.Module] -> String
do_test_hg ast = case go of
        Left s -> s
        Right x -> x
    where
    go = do
        asts <- build_gtrees ast
        let graphs = map build asts
        return $ unlines $ map S.ast_show graphs








