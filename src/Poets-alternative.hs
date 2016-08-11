{-# LANGUAGE TypeFamilies, RankNTypes, FlexibleContexts #-}

module Poets where

class (Vertex (GraphVertex g), Edge (GraphEdge g)) => Graph g where
    data GraphProperties g
    data GraphVertex g
    data GraphEdge g
    graphProperties :: g -> GraphProperties g
    graphVertices   :: g -> [GraphVertex g]
    graphEdges      :: g -> [GraphEdge g]

class Ord v => Vertex v where
    type VertexProperties v
    type VertexState v
    type VertexInputPort v
    type VertexOutputPort v
    vertexProperties :: v -> VertexProperties v
    vertexState      :: v -> VertexState v
    setVertexState   :: VertexState v -> v -> v
    inputPorts       :: v -> [VertexInputPort v]
    outputPorts      :: v -> [VertexOutputPort v]

class Ord e => Edge e where
    type EdgeProperties e
    type EdgeState e
    type EdgeMessage e
    edgeProperties :: e -> EdgeProperties e
    edgeState      :: e -> EdgeState e
    setEdgeState   :: EdgeState e -> e -> e

class (Vertex (InputPortVertex i), Edge (InputPortEdge i)) => InputPort i where
    type InputPortVertex i
    type InputPortEdge i
    inputPortVertex :: i -> InputPortVertex i
    inputPortEdge   :: i -> InputPortEdge i
    receiveHandler  :: ( InputPortVertex i ~ GraphVertex g
                       , InputPortEdge   i ~ GraphEdge g )
                    => i
                    -> ReceiveHandler g m

class (Vertex (InputPortVertex o), Edge (InputPortEdge o)) => OutputPort o where
    type OutputPortVertex o
    type OutputPortEdge o
    outputPortVertex :: o -> OutputPortVertex o
    outputPortEdge   :: o -> OutputPortEdge o
    sendHandler      :: ( OutputPortVertex o ~ GraphVertex g
                        , OutputPortEdge   o ~ GraphEdge g )
                     => o
                     -> SendHandler g m

type ReadyToSend v = [VertexOutputPort v]

type ReceiveHandler g m = (Graph g, Monad m)
                        => GraphProperties g
                        -> GraphVertex g
                        -> GraphEdge g
                        -> EdgeMessage (GraphEdge g)
                        -> m ( VertexState (GraphVertex g)
                             , EdgeState   (GraphEdge   g)
                             , ReadyToSend (GraphVertex g) )

trivialReceiveHandler :: ReceiveHandler g m
trivialReceiveHandler _ v e _ = return (vertexState v, edgeState e, [])

type SendHandler g m = (Graph g, Monad m)
                     => GraphProperties g
                     -> GraphVertex g
                     -> GraphEdge g
                     -> m ( Maybe (EdgeMessage (GraphEdge g))
                          , VertexState (GraphVertex g)
                          , EdgeState   (GraphEdge   g)
                          , ReadyToSend (GraphVertex g) )

trivialSendHandler :: SendHandler g m
trivialSendHandler _ v e = return (Nothing, vertexState v, edgeState e, [])
