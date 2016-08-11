{-# LANGUAGE TypeFamilies, RankNTypes, FlexibleContexts #-}

module Poets where

-- Generic type families for properties and states of vertices, edges, etc.
-- TODO: Maybe change to data families?
type family Properties a
type family State a

-- All data types are parameterised by a: the application type.
data Graph a = Graph
    { graphProperties :: Properties (Graph a)
    , graphVertices   :: [Vertex a]
    , graphEdges      :: [Edge a] }

data Vertex a = Vertex
    { vertexProperties :: Properties (Vertex a)
    , vertexState      :: State (Vertex a)
    , inputPorts       :: [InputPort a]
    , outputPorts      :: [OutputPort a] }

updateVertexState :: State (Vertex a) -> Vertex a -> Vertex a
updateVertexState s (Vertex p _ is os) = Vertex p s is os

data Edge a = Edge
    { edgeProperties :: Properties (Edge a)
    , edgeState      :: State (Edge a) }

updateEdgeState :: State (Edge a) -> Edge a -> Edge a
updateEdgeState s (Edge p _) = Edge p s

data InputPort a = InputPort
    { inputPortVertex :: Vertex a
    , inputPortEdge   :: Edge a }

data OutputPort a = OutputPort
    { outputPortVertex :: Vertex a
    , outputPortEdge   :: Edge a }

type ReceiveHandler a m = Monad m => Properties (Graph a)
                                  -> InputPort a
                                  -> m ( State (Vertex a)
                                       , State (Edge a)
                                       , [OutputPort a] )

trivialReceiveHandler :: ReceiveHandler a m
trivialReceiveHandler _ i = return ( vertexState (inputPortVertex i)
                                   , edgeState   (inputPortEdge   i)
                                   , [] )

type SendHandler a m = Monad m => Properties a
                               -> OutputPort a
                               -> m ( Maybe Bool
                                    , State (Vertex a)
                                    , State (Edge   a)
                                    , [OutputPort a] )

trivialSendHandler :: SendHandler a m
trivialSendHandler _ o = return ( Nothing
                                , vertexState (outputPortVertex o)
                                , edgeState   (outputPortEdge   o)
                                , [] )



data Etx = Etx { etxNetworkName :: String }

data EtxProperties = EtxProperties { numVertices :: Int, numEdges :: Int }

type instance Properties Etx = EtxProperties


-- orchestrate :: Monad m
--             => graphProperties
--             -> [Vertex]
--             ->
--             ->

-- orchestrate = undefined
