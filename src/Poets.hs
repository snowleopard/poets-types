{-# LANGUAGE TypeFamilies, RankNTypes, FlexibleContexts #-}

module Poets where

-- All data types are parameterised by g: the type of the application graph.

type family Properties g
type family State g

data Graph g = Graph
    { graphProperties :: Properties g
    , graphVertices   :: [Vertex g]
    , graphEdges      :: [Edge g] }

data Vertex g = Vertex
    { vertexProperties :: Properties (Vertex g)
    , vertexState      :: State (Vertex g)
    , inputPorts       :: [InputPort (Vertex g)]
    , outputPorts      :: [OutputPort (Vertex g)] }

updateVertexState :: State (Vertex g) -> Vertex g -> Vertex g
updateVertexState s (Vertex p _ is os) = Vertex p s is os

data Edge g = Edge
    { edgeProperties :: Properties (Edge g)
    , edgeState      :: State (Edge g) }

updateEdgeState :: State (Edge g) -> Edge g -> Edge g
updateEdgeState s (Edge p _) = Edge p s

data InputPort g = InputPort
    { inputPortVertex :: Vertex g
    , inputPortEdge   :: Edge g
    , receiveHandler  :: ReceiveHandler g }

data OutputPort g = OutputPort
    { outputPortVertex :: Vertex g
    , outputPortEdge   :: Edge g
    , sendHandler      :: SendHandler g }

type ReceiveHandler g = Properties g
                     -> Vertex g
                     -> Edge g
                     ->   ( State (Vertex g)
                          , State (Edge g)
                          , [InputPort (Vertex g)] )

trivialReceiveHandler :: ReceiveHandler g
trivialReceiveHandler _ v e = (vertexState v, edgeState e, [])

type SendHandler g = Properties g
                   -> Vertex g
                   -> Edge g
                   ->   ( Maybe Bool
                        , State (Vertex g)
                        , State (Edge   g)
                        , [InputPort (Vertex g)] )

trivialSendHandler :: SendHandler g
trivialSendHandler _ v e = (Nothing, vertexState v, edgeState e, [])



data Etx = Etx { etxNetworkName :: String }

data EtxProperties = EtxProperties { numVertices :: Int, numEdges :: Int }

type instance Properties Etx = EtxProperties


-- orchestrate :: Monad m
--             => graphProperties
--             -> [Vertex]
--             ->
--             ->

-- orchestrate = undefined
