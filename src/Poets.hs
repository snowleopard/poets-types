{-# LANGUAGE TypeFamilies, RankNTypes, GADTs, FlexibleInstances #-}

module Poets where

--import Control.Monad.State.Lazy

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
    -- TrainInput :: InputState Train -> Output Train -> Vertex -> Input Train
    -- AirInput   :: InputState Air   -> Output Air   -> Vertex -> Input Air

-- inputPreset :: Input p -> Output p
-- inputPreset

data Output p where
    TrainOutput :: OutputState Train -> Vertex -> Output Train
    AirOutput   :: OutputState Air   -> Vertex -> Output Air

kgxCross :: Input Train
kgxCross = TrainInput 0 london

nclCentral :: Output Train
nclCentral = TrainOutput 0 newcastle

nclAirport :: Output Air
nclAirport = AirOutput "toon" newcastle

southamptonAirport :: Input Air
southamptonAirport = AirInput "city" southampton

data AbstractInput where
    AbstractInput :: Input p -> AbstractInput

data AbstractOutput where
    AbstractOutput :: Output p -> AbstractOutput

type RTS = [AbstractOutput]

type ReceiveHandler m = forall p. Input p -> Packet p -> m (InputState p, RTS)

type SendHandler m = forall p. Output p -> m (Maybe (Packet p), OutputState p, RTS)

receive :: Monad m => ReceiveHandler m
receive (TrainInput s v) packet = return (s + packet + vState v, [])
receive (AirInput   s v) packet = return (s ++ packet ++ vLabel v, [])

send :: Monad m => SendHandler m
send (TrainOutput s v) = return (Just 8, s + vState v, [])
send (AirOutput   s v) = return (Just "Hi", s ++ vLabel v, [])

data Event where
    InputEvent  :: Input  p -> Packet p -> Event
    OutputEvent :: Output p -> Event

type Topology = forall p. Output p -> [Input p]

go :: Monad m => ReceiveHandler m -> SendHandler m -> Topology -> Event -> m [Event]
go r _ _ (InputEvent  i packet) = do _ <- r i packet
                                     return []
go _ s t (OutputEvent o) = do (maybePacket, _, _) <- s o
                              return $ case maybePacket of
                                  Nothing     -> []
                                  Just packet -> [ InputEvent i packet | i <- t o ]

data Edge where
    Edge :: Output p -> [Packet p] -> Input p -> Edge

edges :: [Edge]
edges = [ Edge nclCentral [1, 2, 3] kgxCross
        , Edge nclAirport ["plane"] southamptonAirport ]

test :: Monad m => m ()
test = orchestrate edges 10

orchestrate :: Monad m => [Edge] -> Int -> m ()
orchestrate es n
    | n <= 0    = return ()
    | otherwise = orchestrate es (n - 1)

data Graph = Graph
    { vertices :: [Vertex]
    , inputs   :: [AbstractInput]
    , outputs  :: [AbstractOutput]
    , preset   :: AbstractInput  -> AbstractOutput
    , postset  :: AbstractOutput -> [AbstractInput] }

buildGraph :: [Edge] -> Graph
buildGraph es = Graph vs is os pre post
  where
    is   = undefined -- map (\(Edge i _) -> AbstractInput i) es
    os   = undefined -- map (\AbstractInput i -> AbstractOutput $ )
    vs   = undefined
    pre  = undefined
    post = undefined
