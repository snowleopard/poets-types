module Poets where

newtype Id = Id { fromId :: Int } deriving (Eq, Show)

-- Device type is parameterised by the types of properties (p) and state (s)
data Device p s = Device
    { deviceId         :: Id
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
type ReceiveHandler g d ds e es = g -> d -> ds -> e -> es -> (ds, es, RTS)


-- data Edge s p
