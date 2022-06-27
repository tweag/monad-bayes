{-# LANGUAGE BlockArguments #-}

module NestedInference where

import Control.Monad.Bayes.Class (MonadInfer, MonadSample, factor)
import Control.Monad.Bayes.Enumerator (mass)
import Numeric.Log (Log (Exp))

-- data Utterance = ASquare | AShape deriving (Eq, Show, Ord)

-- data State = Square | Circle deriving (Eq, Show, Ord)

-- data Action = Speak Utterance | DoNothing deriving (Eq, Show, Ord)

-- -- | uniformly likely to say any true utterance to convey the given state
-- truthfulAgent :: MonadSample n m => State -> m Action
-- truthfulAgent state = uniformD case state of
--   Square -> [Speak ASquare, Speak AShape, DoNothing]
--   Circle -> [Speak AShape, DoNothing]

-- -- | a listener which applies Bayes rule to infer the state
-- -- given an observed action of the other agent
-- listener :: MonadInfer n m => Action -> m State
-- listener observedAction = do
--   state <- uniformD [Square, Circle]
--   factor $ log $ Exp $ mass (truthfulAgent state) observedAction
--   return state

-- -- | an agent which produces an action by reasoning about
-- -- how the listener would interpret it
-- informativeAgent :: MonadInfer n m => State -> m Action
-- informativeAgent state = do
--   utterance <- uniformD [Speak ASquare, Speak AShape, DoNothing]
--   factor $ log $ Exp $ mass (listener utterance) state
--   return utterance
