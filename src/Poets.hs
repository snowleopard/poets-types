{-# LANGUAGE TypeFamilies, RankNTypes, GADTs #-}

module Poets where

--import Control.Monad.State.Lazy

-- class Poets l s a where
--     getLabel :: Poets l s l
--     getState :: Poets l s s
--     setState :: s -> Poets l s ()

-- Orchestrator needs to set local st
-- s ~ (VertexState, EdgeState)
-- o ~ (Vertex, Edge t)

-- type ReceiveHandler t o s m = MonadState s m => t -> m [o]

-- type SendHandler t o s m = MonadState s m => m (Maybe t, [o])

-- trivialReceiveHandler :: ReceiveHandler t o s m
-- trivialReceiveHandler _ = return []

-- trivialSendHandler :: SendHandler t o s m
-- trivialSendHandler = return (Nothing, [])

-- orchestrate :: Monad m
--             => graphProperties
--             -> [Vertex]
--             ->
--             ->

-- orchestrate = undefined

-- Orchestrator orchestrates events:
--data Event p = Receive (Input p) p | Send (Output p) p

data Vertex = Vertex String

-- data Input p = Input (InputState p) Vertex

data Output p
type family InputState p
data OutputState p

data Receive p = Receive (Input  p) p
data Send    p = Send    (Output p) p

data AbstractOutput where
    AbstractOutput :: Output p -> AbstractOutput

type RTS = [AbstractOutput]

type ReceiveHandler p m = Input p -> p -> m (InputState p, RTS)

type SendHandler p m = Output p -> m (Maybe p, OutputState p, RTS)


-- City/ports analogy:

newcastle :: Vertex
newcastle = Vertex "Newcastle"

london :: Vertex
london = Vertex "London"

southampton :: Vertex
southampton = Vertex "Southampton"

data Airport
data TrainStation
data Seaport

type instance InputState TrainStation = Int
type instance InputState Airport      = String

-- Add Output p as a source field of Input p
data Input p where
    TrainInput   :: InputState TrainStation -> Vertex -> Input TrainStation
    AirportInput :: InputState Airport      -> Vertex -> Input Airport

kgxCross :: Input TrainStation
kgxCross = TrainInput 0 london

test :: Monad m => ReceiveHandler p m
test (TrainInput   s v) packet = return (s + 1, [])
test (AirportInput s v) packet = return (s ++ "!", [])
