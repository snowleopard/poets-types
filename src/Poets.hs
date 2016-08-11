{-# LANGUAGE TypeFamilies, RankNTypes, FlexibleContexts #-}

module Poets where

type family GraphVertex g
type family GraphEdge g
type family VertexProperties g
type family EdgeProperties g


type family State g
type family Properties g
type family VertexInputPort g
type family VertexOutputPort g

data Graph g = Graph
    { graphProperties :: Properties g
    , graphVertices   :: [GraphVertex g]
    , graphEdges      :: [GraphEdge g] }

data Vertex g = Vertex
    { vertexProperties :: Properties (GraphVertex g)
    , vertexState      :: State (GraphVertex g)
    , inputPorts       :: [VertexInputPort (GraphVertex g)]
    , outputPorts      :: [VertexOutputPort (GraphVertex g)] }

updateVertexState :: State (GraphVertex g) -> Vertex g -> Vertex g
updateVertexState s (Vertex p _ is os) = Vertex p s is os

data Edge g = Edge
    { edgeProperties :: Properties (GraphEdge g)
    , edgeState      :: State (GraphEdge g) }

updateEdgeState :: State (GraphEdge g) -> Edge g -> Edge g
updateEdgeState s (Edge p _) = Edge p s

data InputPort g m = InputPort
    { inputPortVertex :: Vertex g
    , inputPortEdge   :: Edge g
    , receiveHandler  :: ReceiveHandler g m }

type ReceiveHandler g m = (GraphVertex g ~ Vertex g, GraphEdge g ~ Edge g, Monad m)
                      => Properties g
                      -> Vertex g
                      -> Edge g
                      -> m ( State (Vertex g)
                           , State (Edge g)
                           , [VertexInputPort (GraphVertex g)] )

trivialReceiveHandler :: ReceiveHandler g m
trivialReceiveHandler _ v e = return (vertexState v, edgeState e, [])

-- class (Vertex (InputPortVertex o), Edge (InputPortEdge o)) => OutputPort o where
--     type OutputPortVertex o
--     type OutputPortEdge o
--     outputPortVertex :: o -> OutputPortVertex o
--     outputPortEdge   :: o -> OutputPortEdge o
--     sendHandler      :: ( OutputPortVertex o ~ GraphVertex g
--                         , OutputPortEdge   o ~ GraphEdge g )
--                      => o
--                      -> SendHandler g m

-- type SendHandler g m = (Graph g, Monad m)
--                      => GraphProperties g
--                      -> GraphVertex g
--                      -> GraphEdge g
--                      -> m ( Maybe (EdgeMessage (GraphEdge g))
--                           , VertexState (GraphVertex g)
--                           , EdgeState   (GraphEdge   g)
--                           , ReadyToSend (GraphVertex g) )

-- trivialSendHandler :: SendHandler g m
-- trivialSendHandler _ v e = return (Nothing, vertexState v, edgeState e, [])


-- orchestrate :: Monad m
--             => graphProperties
--             -> [Vertex]
--             ->
--             ->

-- orchestrate = undefined
