{-# LANGUAGE TypeFamilies, RankNTypes, GADTs, FlexibleContexts #-}

module Poets where

import Data.List.Extra
import qualified Data.Map
--import Control.Monad.State.Lazy

-- Discuss:
-- * Safe graph spec
-- * Make RTS part of vertex state
-- * Delays, time-dependent ready-to-send

-- Make invalid graphs non-representable:
-- * A port belongs to exactly one vertex
-- * An edge connects input and output ports of the same type
-- * An input port has exactly one source

-- Examples that don't work:
-- * Vertex lists ports: same port belongs to different vertices
-- * Circular parent/children links (can be inconsistent)
-- * Edge list can assign multiple sources to an input port

data Vertex = Vertex
    { vLabel :: String
    , vState :: Int }

-- City/ports analogy:

newcastle :: Vertex
newcastle = Vertex "Newcastle" 0

london :: Vertex
london = Vertex "London" 0

southampton :: Vertex
southampton = Vertex "Southampton" 0

data Air
data Train
data Sea

-- type family InputState p
-- type family OutputState p
type family Packet p

-- type instance InputState  Train = Int
-- type instance OutputState Train = Int
type instance Packet      Train = Int

-- type instance InputState  Air = String
-- type instance OutputState Air = String
type instance Packet      Air = String

data TInput p where
    TInput :: { iState  :: State (TInput p)
              , iSource :: TOutput p
              , iVertex :: Vertex } -> TInput p

data TOutput p where
    TOutput :: { oState  :: State (TOutput p)
               , oVertex :: Vertex } -> TOutput p

kgxCross :: TInput Train
kgxCross = TInput 0 nclCentral london

nclCentral :: TOutput Train
nclCentral = TOutput 0 newcastle

nclAirport :: TOutput Air
nclAirport = TOutput (SOA "toon") newcastle

southamptonAirport :: TInput Air
southamptonAirport = TInput (SIA "city") nclAirport southampton

data Input where
    Input :: TInput p -> Input

data Output where
    Output :: TOutput p -> Output

-- data Graph v i o = Graph
--     { gInputs       :: [forall p. i p]
--     , gSource       :: forall p. i p -> o p
--     , gInputVertex  :: forall p. i p -> v
--     , gOutputVertex :: forall p. o p -> v }

-- data GraphState v i o = GraphState
--     { gvState :: v -> State v
--     , giState :: forall p. i p -> State (i p)
--     , goState :: forall p. o p -> State (o p) }

data family State a

data instance State (TInput Train) = SIT { fromSIT :: Int }
data instance State (TInput Air)   = SIA String
data instance State (TOutput Train) = SOT Int
data instance State (TOutput Air)   = SOA String

-- data Event i where
--     Event :: i -> p -> Event i

-- receive :: Monad m => Receive m
-- receive (TInput s o v)
-- receive (TrainInput s _ v) packet = return (SIT $ fromSIT s + packet + vState v, vState v)
-- receive (AirInput   s _ v) packet = return (s ++ packet ++ vLabel v, vState v)

-- send :: Monad m => Send m
-- send (TrainOutput s v) = return (Just 8, s + vState v, [])
-- send (AirOutput   s v) = return (Just "Hi", s ++ vLabel v, [])

-- Events:
-- Input receives a packet: i -> p -> m (State i, State v)
-- Output sends a packet, creating new events: o -> m (Maybe p, State o, State v)
-- Enabled outputs -- part of a vertex state

type Receive m v i = forall p. i p -> p -> m (State (i p), State v)

type Send m v o = forall p. o p -> m (Maybe p, State (o p), State v)

-- orchestrate :: Monad m
--             => Graph v i o
--             -> GraphState v i o
--             -> [e]
--             -> Receive m e
--             -> Send m e
--             -> m (GraphState v i o, [e])
-- orchestrate = undefined

-- Connectivity:
-- TInput -> TOutput
-- TInput -> Vertex
-- TOutput -> Vertex
--
-- inverse:
-- Vertex -> ([TInput], [TOutput])
-- TOutput -> [TInput]

-- type RTS = [Output]

-- type ReceiveHandler m = forall p. TInput p -> Packet p -> m (InputState p, RTS)

-- type SendHandler m = forall p. TOutput p -> m (Maybe (Packet p), OutputState p, RTS)

-- data InputEvent where
--     InputEvent :: TInput  p -> Packet p -> Event

-- data OutputEvent where
--     OutputEvent :: TInput  p -> Packet p -> Event

-- data Event where
--     InputEvent  :: TInput  p -> Packet p -> Event
--     OutputEvent :: TOutput p -> Event

-- type Topology = forall p. TOutput p -> [TInput p]

-- handle :: Monad m => ReceiveHandler m -> SendHandler m -> Topology -> Event -> m [Event]
-- handle r _ _ (InputEvent  i packet) = do _ <- r i packet
--                                          return []
-- handle _ s t (OutputEvent o) = do (maybePacket, _, _) <- s o
--                                   return $ case maybePacket of
--                                       Nothing     -> []
--                                       Just packet -> [ InputEvent i packet | i <- t o ]

-- data Edge where
--     Edge :: TOutput p -> [TInput p] -> Edge

-- edges :: [Edge]
-- edges = [ Edge nclCentral [1, 2, 3] kgxCross
--         , Edge nclAirport ["plane"] southamptonAirport ]

-- test :: Monad m => m ()
-- test = orchestrate edges 10

-- orchestrate :: Monad m => [Edge] -> Int -> m ()
-- orchestrate es n
--     | n <= 0    = return ()
--     | otherwise = orchestrate es (n - 1)

data ExpandedGraph = ExpandedGraph
    { vertices :: [Vertex]
    , inputs   :: [Input]
    , outputs  :: [Output]
    , preset   :: Input  -> Output
    , postset  :: Output -> [Input] }

buildGraph :: Ord Output => [Input] -> ExpandedGraph
buildGraph is = ExpandedGraph vs is os pre post
  where
    os   = map pre is
    vs   = map (\(Input  i) -> inputVertex i) is
        ++ map (\(Output o) -> outputVertex o) os
    pre  = \(Input i) -> Output (inputPreset i)
    post o = Data.Map.findWithDefault [] o $ Data.Map.fromAscList dict
    dict = groupSort [ (pre i, i) | i <- is ]

-- type EventPool = [Event]

-- strategy :: EventPool -> Maybe (Event, EventPool)
-- strategy = undefined

-- step :: Monad m => EventPool -> ReceiveHandler m -> SendHandler m -> Topology -> m (Maybe EventPool)
-- step pool r s t = case strategy pool of
--     Nothing        -> return Nothing
--     Just (e, rest) -> do
--         new <- handle r s t e
--         return . Just $ rest ++ new

-- Pool of events.
-- Oracle pulls events out of the pool one by one. The oracle terminates the
-- computation by pulling out Nothing instead of an event.
-- class Pool a where
--     type PoolEvent a
--     empty  :: a
--     add    :: [PoolEvent a] -> a
--     oracle :: a -> Maybe (a, p)
