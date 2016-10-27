{-# LANGUAGE RankNTypes, GADTs #-}

module Poets where

import Control.Monad.State.Lazy

-- Receive handler for an input port of type 'i p' that accepts packets of
-- type 'p' and does some computation in a monad 'm'.
data Receive i m where
    Receive :: Monad m => (i p -> p -> m ()) -> Receive i m

-- Send handler for an output port of type 'o p' that does some computation in
-- a monad 'm' to decide whether to send a packet of type 'p' or not (Nothing).
data Send o m where
    Send :: Monad m => (o p -> m (Maybe p)) -> Send o m

-- A generic receive handler that ignores all incoming packets.
deaf :: Monad m => Receive i m
deaf = Receive $ \_ _ -> return ()

-- A generic send handler that never sends any packets.
mute :: Monad m => Send o m
mute = Send $ \_ -> return Nothing

-- Pub example:

-- Pub's state is the number of pints that have been ordered.
type Pub = StateT Int IO

-- Just print a message to the screen in the IO monad.
say :: String -> Pub ()
say = lift . putStrLn

-- Our pub has two "input ports": one accepts orders for beer, another simply
-- listens to a customer who tells a story.
data PubInput a where
    Order :: PubInput Int
    Story :: PubInput String

-- Pub's only output port produces packets of type Beer (a number of pints of)
newtype Beer = Beer Int

data PubOutput a where
    Bar :: PubOutput Beer

-- Announces an order or a story, changes the state of the pub by adding the
-- ordered beer quantity to internal memory.
pubReceiveHandler :: Receive PubInput Pub
pubReceiveHandler = Receive handle
  where
    handle :: forall p. PubInput p -> p -> Pub ()
    handle Order n = do
        modify (+n)
        say $ "We need " ++ show n ++ " more pints here!"
    handle Story s = say $ "Here is an interesting story: " ++ s

-- Sends out a packet full of ordered beer or does nothing if no beer has been
-- ordered so far.
pubSendHandler :: Send PubOutput Pub
pubSendHandler = Send handle
  where
    handle :: forall p. PubOutput p -> Pub (Maybe p)
    handle Bar = do
        ordered <- get
        put 0
        return $ if ordered > 0 then Just (Beer ordered) else Nothing

-- An edge connecting an output and an input port whose packet types match.
data Edge where
    Edge :: o p -> i p -> Edge

-- Now let's add some customers...

-- Andrey can order a beer
data AndreyOutput a where
    OrderBeer :: AndreyOutput Int

e1 :: Edge
e1 = Edge OrderBeer Order

andreySendHandler :: Send AndreyOutput Pub
andreySendHandler = Send $ \OrderBeer -> do
    say "Andrey orders a beer"
    return $ Just 1

-- Andrew can tell a story about POETS
data AndrewOutput a where
    TellStory :: AndrewOutput String

e2 :: Edge
e2 = Edge TellStory Story

andrewSendHandler :: Send AndrewOutput Pub
andrewSendHandler = Send $ \TellStory -> do
    say "Andrew tells a story"
    return $ Just "POETS is a long story! Let me start with..."

-- David listen to Andrew's stories
data DavidInput a where
    Listen :: DavidInput String

e3 :: Edge
e3 = Edge TellStory Listen

davidReceiveHandler :: Receive DavidInput Pub
davidReceiveHandler = Receive $ \Listen s -> do
    say $ "David heard a story: " ++ s
    return ()

-- Simon drinks beer
data SimonInput a where
    Drink :: SimonInput Beer

e4 :: Edge
e4 = Edge Bar Drink

simonReceiveHandler :: Receive SimonInput Pub
simonReceiveHandler = Receive $ \Drink (Beer n) -> do
    say $ "Simon drinks " ++ show n ++ " pints of beer"
    return ()

edges :: [Edge]
edges = [e1, e2, e3, e4]

-- We also need to define generic input and output port types
data InputPort m where
    PubInputPort   :: PubInput   p -> Receive PubInput   Pub -> InputPort Pub
    DavidInputPort :: DavidInput p -> Receive DavidInput Pub -> InputPort Pub
    SimonInputPort :: SimonInput p -> Receive SimonInput Pub -> InputPort Pub

data OutputPort m where
    PubOutputPort    :: PubOutput    p -> Send PubOutput    Pub -> OutputPort Pub
    AndreyOutputPort :: AndreyOutput p -> Send AndreyOutput Pub -> OutputPort Pub
    AndrewOutputPort :: AndrewOutput p -> Send AndrewOutput Pub -> OutputPort Pub

-- We now list all input and output ports in our graph, along with the handlers
inputPorts :: [InputPort Pub]
inputPorts = [ PubInputPort   Order  pubReceiveHandler
             , PubInputPort   Story  pubReceiveHandler
             , DavidInputPort Listen davidReceiveHandler
             , SimonInputPort Drink  simonReceiveHandler ]

outputPorts :: [OutputPort Pub]
outputPorts = [ PubOutputPort    Bar       pubSendHandler
              , AndreyOutputPort OrderBeer andreySendHandler
              , AndrewOutputPort TellStory andrewSendHandler ]

data Event where
    InputEvent  :: i p -> p -> Event
    OutputEvent :: o p -> p -> Event

-- A simple generic orchestrator
simple :: [Edge] -> [Event] -> [InputPort m] -> [OutputPort m] -> m ()
simple g es is os todo = when (todo > 0 && not (null es)) $ do


