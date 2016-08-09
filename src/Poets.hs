{-# LANGUAGE TypeFamilies, Rank2Types #-}

module Poets where

import Data.Set

-- import Control.Monad.RWS.Lazy
-- data DeviceId

-- Parameters:
-- * Graph properties: graph
-- * Device properties: device
-- * Edge properties: edge
-- * Device state: deviceState
-- * Edge state: edgeState

data DeviceId
data EdgeId
data InputPortId
data OutputPortId

class Device d where
    type DeviceProperties d
    type DeviceState d
    type DeviceInputPort d
    type DeviceOutputPort d
    deviceId         :: d -> DeviceId
    deviceProperties :: d -> DeviceProperties d
    deviceState      :: d -> DeviceState d
    setDeviceState   :: DeviceState d -> d -> d
    inputPorts       :: Set (DeviceInputPort d)
    outputPorts      :: Set (DeviceOutputPort d)

class Edge e where
    type EdgeProperties e
    type EdgeState e
    edgeId         :: e -> EdgeId
    edgeProperties :: e -> EdgeProperties e
    edgeState      :: e -> EdgeState e
    setEdgeState   :: EdgeState e -> e -> e

type ReceiveHandler g d e =
       (Device d, Edge e) => g
                          -> d
                          -> e
                          -> (DeviceState d, EdgeState e, [DeviceOutputPort d])

-- Device type is parameterised by the types of properties (p) and state (s)
-- data Device p s = Device
--     { deviceId         :: DeviceId
--     , deviceProperties :: p
--     , deviceState      :: s
--     , deviceInputs     :: [InputPort]
--     , deviceOutputs    :: [OutputPort] }

-- data InputPortId
-- data OutputPortId
-- data Edge

-- data InputPort = InputPort
--     { inputPortId :: InputPortId
--     , inputEdge   :: Edge
--     , inputDevice :: Device }

-- data OutputPort = OutputPort
--     { outputPortId :: OutputPortId
--     , outputEdge   :: Edge
--     , outputDevice :: Device }

-- -- data Port a where
-- --     InputPort ::
-- -- data OutputPort
-- data RTS

-- -- * g -- graph properties
-- -- * d -- device properties
-- -- * s -- device state
-- -- * e --
-- -- type ReceiveHandler = Graph gp -> Device dp ds do -> Edge ep es -> (ds, es, [do])

-- -- data Edge s p

-- -- data Pin = Pin
-- --     { deviceId :: DeviceId
-- --     , edgeId   :: EdgeId }


-- -- An event is a message that arrived via an edge.
-- data Event e a = Event
--     { edge    :: e
--     , message :: a }

-- data GraphProperties
-- data VertexProperties
-- data EdgeProperties

-- data Context = Context
--     { graphProperties  :: GraphProperties
--     , vertexProperties :: VertexProperties
--     , edgeProperties   :: EdgeProperties }

-- data State
-- data Log

-- type VertexComputation m a = RWST Context Log State m a

-- type EventHandler m a = Event -> VertexComputation m a
