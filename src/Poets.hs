module Poets where

import Control.Monad.RWS.Lazy

data DeviceId

-- Device type is parameterised by the types of properties (p) and state (s)
data Device p s = Device
    { deviceId         :: DeviceId
    , deviceProperties :: p
    , deviceState      :: s
    , deviceInputs     :: [InputPort]
    , deviceOutputs    :: [OutputPort] }

data InputPort
data OutputPort
data RTS

-- * g -- graph properties
-- * d -- device properties
-- * s -- device state
-- * e --
-- type ReceiveHandler = Graph gp -> Device dp ds do -> Edge ep es -> (ds, es, [do])

-- data Edge s p

-- data Pin = Pin
--     { deviceId :: DeviceId
--     , edgeId   :: EdgeId }


-- An event is a message that arrived via an edge.
data Event e a = Event
    { edge    :: e
    , message :: a }

data GraphProperties
data VertexProperties
data EdgeProperties

data Context = Context
    { graphProperties  :: GraphProperties
    , vertexProperties :: VertexProperties
    , edgeProperties   :: EdgeProperties }

data State
data Log

type VertexComputation m a = RWST Context Log State m a

type EventHandler m a = Event -> VertexComputation m a
