{-# LANGUAGE TypeFamilies, RankNTypes, GADTs #-}

module Poets where

-- Generic type families for properties and states of vertices, edges, etc.
-- TODO: Maybe change to data families?
type family Properties a
type family State a

-- All data types are parameterised by a: the application type.
data Graph a = Graph
    { graphProperties :: Properties (Graph a)
    , graphVertices   :: [Vertex a]
    , graphEdges      :: [AbstractEdge a] }

data Vertex a = Vertex
    { vertexProperties :: Properties (Vertex a)
    , vertexState      :: State      (Vertex a)
    , vertexPorts      :: [AbstractPort a] }

-- preset :: Vertex a -> [Edge a e]
-- preset (Vertex _ _ ps) = catMaybes getPresetEdge ps

-- getPresetEdge :: AbstractPort a -> Edge a e
-- getPresetEdge (Input (InputPort _ e)) = Just e
-- getPresetEdge _                       = Nothing

updateVertexState :: State (Vertex a) -> Vertex a -> Vertex a
updateVertexState s (Vertex p _ ps) = Vertex p s ps

-- Edges are parameterised by phantom edge type e
data Edge a e = Edge
    { edgeProperties :: Properties (Edge a e)
    , edgeState      :: State      (Edge a e) }

-- Edges of type e accept messages of type Message e
type family Message e

data family AbstractEdge a

updateEdgeState :: State (Edge a e) -> Edge a e -> Edge a e
updateEdgeState s (Edge p _) = Edge p s

data InputPort a e = InputPort
    { inputPortVertex :: Vertex a
    , inputPortEdge   :: Edge a e }

data OutputPort a e = OutputPort
    { outputPortVertex :: Vertex a
    , outputPortEdge   :: Edge a e }

data AbstractPort a where
    Input  :: InputPort  a e -> AbstractPort a
    Output :: OutputPort a e -> AbstractPort a

type ReceiveHandler a e m = Monad m => Properties (Graph a)
                                    -> InputPort a e
                                    -> Message e
                                    -> m ( State (Vertex a)
                                         , State (Edge a e)
                                         , [OutputPort a e] )

trivialReceiveHandler :: ReceiveHandler a e m
trivialReceiveHandler _ i _ = return ( vertexState (inputPortVertex i)
                                     , edgeState   (inputPortEdge   i)
                                     , [] )

type SendHandler a e m = Monad m => Properties a
                                 -> OutputPort a e
                                 -> m ( Maybe (Message e)
                                      , State (Vertex a)
                                      , State (Edge   a e)
                                      , [OutputPort a e] )

trivialSendHandler :: SendHandler a e m
trivialSendHandler _ o = return ( Nothing
                                , vertexState (outputPortVertex o)
                                , edgeState   (outputPortEdge   o)
                                , [] )



data Etx = Etx { etxFilePath :: FilePath }

data EtxProperties = EtxProperties { numVertices :: Int, numEdges :: Int }

type instance Properties (Graph  Etx) = EtxProperties
type instance Properties (Vertex Etx) = String
type instance State      (Vertex Etx) = Int

data instance AbstractEdge Etx where
    BlueEdge :: Edge Etx Blue -> AbstractEdge Etx
    RedEdge  :: Edge Etx Red  -> AbstractEdge Etx

graph :: Graph Etx
graph = Graph (EtxProperties 2 2) [a, b] [RedEdge e, BlueEdge f]

a :: Vertex Etx
a = Vertex "a" 0 []

b :: Vertex Etx
b = Vertex "b" 0 []

data Red
data Blue

type instance Properties (Edge Etx Red ) = Int
type instance Properties (Edge Etx Blue) = Bool
type instance State      (Edge Etx Red ) = Int
type instance State      (Edge Etx Blue) = Bool

type instance Message Red  = (String, Int)
type instance Message Blue = String

poets2016 :: Message Red
poets2016 = ("POETS", 2016)

hello :: Message Blue
hello = "hello"

e :: Edge Etx Red
e = Edge 1 1

f :: Edge Etx Blue
f = Edge False False

receive :: ReceiveHandler Etx e m
receive properties (InputPort vertex edge) message = case edge of
    RedEdge  -> return ( vertexState vertex + 1
                            , edgeState edge + 1
                            , [] )
    BlueEdge -> return ( vertexState vertex + numVertices properties
                            , True
                            , [] )

-- type ReceiveHandler a e m = Monad m => Properties (Graph a)
--                                     -> InputPort a e
--                                     -> Message e
--                                     -> m ( State (Vertex a)
--                                          , State (Edge a e)
--                                          , [OutputPort a e] )

-- trivialReceiveHandler :: ReceiveHandler a e m
-- trivialReceiveHandler _ i _ = return ( vertexState (inputPortVertex i)
--                                      , edgeState   (inputPortEdge   i)
--                                      , [] )

-- type SendHandler a e m = Monad m => Properties a
--                                  -> OutputPort a e
--                                  -> m ( Maybe (Message e)
--                                       , State (Vertex a)
--                                       , State (Edge   a e)
--                                       , [OutputPort a e] )


-- orchestrate :: Monad m
--             => graphProperties
--             -> [Vertex]
--             ->
--             ->

-- orchestrate = undefined
