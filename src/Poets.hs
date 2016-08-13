{-# LANGUAGE TypeFamilies, RankNTypes, GADTs #-}

module Poets where

import Control.Monad.State.Lazy

class Poets l s a where
    getLabel :: Poets l s l
    getState :: Poets l s s
    setState :: s -> Poets l s ()

-- Orchestrator needs to set local st
-- s ~ (VertexState, EdgeState)
-- o ~ (Vertex, Edge t)

type ReceiveHandler t o s m = MonadState s m => t -> m [o]

type SendHandler t o s m = MonadState s m => m (Maybe t, [o])

trivialReceiveHandler :: ReceiveHandler t o s m
trivialReceiveHandler _ = return []

trivialSendHandler :: SendHandler t o s m
trivialSendHandler = return (Nothing, [])

-- orchestrate :: Monad m
--             => graphProperties
--             -> [Vertex]
--             ->
--             ->

-- orchestrate = undefined
