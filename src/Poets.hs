{-# LANGUAGE TypeFamilies, RankNTypes, GADTs #-}

module Poets where

--import Control.Monad.State.Lazy

-- orchestrate :: Monad m
--             => graphProperties
--             -> [Vertex]
--             ->
--             ->

-- orchestrate = undefined

-- Orchestrator orchestrates events:
--data Event p = Receive (Input p) p | Send (Output p) p

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

type family InputState p
type family OutputState p
type family Packet p

type instance InputState  Train = Int
type instance OutputState Train = Int
type instance Packet      Train = Int

type instance InputState  Air = String
type instance OutputState Air = String
type instance Packet      Air = String

data Input p where
    TrainInput :: InputState Train -> Vertex -> Input Train
    AirInput   :: InputState Air   -> Vertex -> Input Air

data Output p where
    TrainOutput :: OutputState Train -> Vertex -> Output Train
    AirOutput   :: OutputState Air   -> Vertex -> Output Air

kgxCross :: AbstractInput
kgxCross = AbstractInput $ TrainInput 0 london

nclAir :: AbstractOutput
nclAir = AbstractOutput $ AirOutput "open" newcastle

data AbstractInput where
    AbstractInput :: Input p -> AbstractInput

data AbstractOutput where
    AbstractOutput :: Output p -> AbstractOutput

type RTS = [AbstractOutput]

type ReceiveHandler p m = Input p -> Packet p -> m (InputState p, RTS)

type SendHandler p m = Output p -> m (Maybe (Packet p), OutputState p, RTS)

test :: Monad m => ReceiveHandler p m
test (TrainInput s v) packet = return (s + packet + vState v, [])
test (AirInput s v) packet = return (s ++ packet ++ vLabel v, [])
