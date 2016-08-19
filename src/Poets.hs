{-# LANGUAGE TypeFamilies, RankNTypes, GADTs, FlexibleContexts #-}

module Poets where

import Data.List.Extra
import qualified Data.Map
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

data TInput p where
    TrainInput :: InputState Train -> TOutput Train -> Vertex -> TInput Train
    AirInput   :: InputState Air   -> TOutput Air   -> Vertex -> TInput Air

inputPreset :: TInput p -> TOutput p
inputPreset (TrainInput _ o _) = o
inputPreset (AirInput   _ o _) = o

inputVertex :: TInput p -> Vertex
inputVertex (TrainInput _ _ v) = v
inputVertex (AirInput   _ _ v) = v

data TOutput p where
    TrainOutput :: OutputState Train -> Vertex -> TOutput Train
    AirOutput   :: OutputState Air   -> Vertex -> TOutput Air

outputVertex :: TOutput p -> Vertex
outputVertex (TrainOutput _ v) = v
outputVertex (AirOutput   _ v) = v

kgxCross :: TInput Train
kgxCross = TrainInput 0 nclCentral london

nclCentral :: TOutput Train
nclCentral = TrainOutput 0 newcastle

nclAirport :: TOutput Air
nclAirport = AirOutput "toon" newcastle

southamptonAirport :: TInput Air
southamptonAirport = AirInput "city" nclAirport southampton

data Input where
    Input :: TInput p -> Input

data Output where
    Output :: TOutput p -> Output

type RTS = [Output]

type ReceiveHandler m = forall p. TInput p -> Packet p -> m (InputState p, RTS)

type SendHandler m = forall p. TOutput p -> m (Maybe (Packet p), OutputState p, RTS)

receive :: Monad m => ReceiveHandler m
receive (TrainInput s _ v) packet = return (s + packet + vState v, [])
receive (AirInput   s _ v) packet = return (s ++ packet ++ vLabel v, [])

send :: Monad m => SendHandler m
send (TrainOutput s v) = return (Just 8, s + vState v, [])
send (AirOutput   s v) = return (Just "Hi", s ++ vLabel v, [])

-- data InputEvent where
--     InputEvent :: TInput  p -> Packet p -> Event

-- data OutputEvent where
--     OutputEvent :: TInput  p -> Packet p -> Event

data Event where
    InputEvent  :: TInput  p -> Packet p -> Event
    OutputEvent :: TOutput p -> Event

type Topology = forall p. TOutput p -> [TInput p]

handle :: Monad m => ReceiveHandler m -> SendHandler m -> Topology -> Event -> m [Event]
handle r _ _ (InputEvent  i packet) = do _ <- r i packet
                                         return []
handle _ s t (OutputEvent o) = do (maybePacket, _, _) <- s o
                                  return $ case maybePacket of
                                      Nothing     -> []
                                      Just packet -> [ InputEvent i packet | i <- t o ]

data Edge where
    Edge :: TOutput p -> [TInput p] -> Edge

-- edges :: [Edge]
-- edges = [ Edge nclCentral [1, 2, 3] kgxCross
--         , Edge nclAirport ["plane"] southamptonAirport ]

-- test :: Monad m => m ()
-- test = orchestrate edges 10

-- orchestrate :: Monad m => [Edge] -> Int -> m ()
-- orchestrate es n
--     | n <= 0    = return ()
--     | otherwise = orchestrate es (n - 1)

data Graph = Graph
    { vertices :: [Vertex]
    , inputs   :: [Input]
    , outputs  :: [Output]
    , preset   :: Input  -> Output
    , postset  :: Output -> [Input] }

buildGraph :: Ord Output => [Input] -> Graph
buildGraph is = Graph vs is os pre post
  where
    os   = map pre is
    vs   = map (\(Input  i) -> inputVertex i) is
        ++ map (\(Output o) -> outputVertex o) os
    pre  = \(Input i) -> Output (inputPreset i)
    post o = Data.Map.findWithDefault [] o $ Data.Map.fromAscList dict
    dict = groupSort [ (pre i, i) | i <- is ]

type EventPool = [Event]

strategy :: EventPool -> Maybe (Event, EventPool)
strategy = undefined

step :: Monad m => EventPool -> ReceiveHandler m -> SendHandler m -> Topology -> m (Maybe EventPool)
step pool r s t = case strategy pool of
    Nothing        -> return Nothing
    Just (e, rest) -> do
        new <- handle r s t e
        return . Just $ rest ++ new

-- Connectivity:
-- TInput -> TOutput
-- TInput -> Vertex
-- TOutput -> Vertex
--
-- inverse:
-- Vertex -> ([TInput], [TOutput])
-- TOutput -> [TInput]
