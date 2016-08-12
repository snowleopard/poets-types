{-# LANGUAGE TypeFamilies, RankNTypes #-}

module Poets where

-- Generic type families for properties and states of vertices, edges, etc.
-- TODO: Maybe change to data families?
type family Properties a
type family State a

-- All data types are parameterised by a: the application type.
data Graph a = Graph
    { graphProperties :: Properties (Graph a)
    , graphVertices   :: [Vertex a]
    , graphEdges      :: forall t. [Edge a t] }

data Vertex a = Vertex
    { vertexProperties :: Properties (Vertex a)
    , vertexState      :: State      (Vertex a)
    , inputPorts       :: forall t. [InputPort a t]
    , outputPorts      :: forall t. [OutputPort a t] }

updateVertexState :: State (Vertex a) -> Vertex a -> Vertex a
updateVertexState s (Vertex p _ is os) = Vertex p s is os

-- Edges are parameterised by phantom edge type t
data Edge a t = Edge
    { edgeProperties :: Properties (Edge a t)
    , edgeState      :: State      (Edge a t) }

-- Edges of type t accept messages of type Message t
type family Message t

updateEdgeState :: State (Edge a t) -> Edge a t -> Edge a t
updateEdgeState s (Edge p _) = Edge p s

data InputPort a t = InputPort
    { inputPortVertex :: Vertex a
    , inputPortEdge   :: Edge a t }

data OutputPort a t = OutputPort
    { outputPortVertex :: Vertex a
    , outputPortEdge   :: Edge a t }

type ReceiveHandler a t m = Monad m => Properties (Graph a)
                                    -> InputPort a t
                                    -> Message t
                                    -> m ( State (Vertex a)
                                         , State (Edge a t)
                                         , [OutputPort a t] )

trivialReceiveHandler :: ReceiveHandler a t m
trivialReceiveHandler _ i _ = return ( vertexState (inputPortVertex i)
                                     , edgeState   (inputPortEdge   i)
                                     , [] )

type SendHandler a t m = Monad m => Properties a
                                 -> OutputPort a t
                                 -> m ( Maybe (Message t)
                                      , State (Vertex a)
                                      , State (Edge   a t)
                                      , [OutputPort a t] )

trivialSendHandler :: SendHandler a t m
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
