module Control.Applicative.List where

-- base

import Control.Applicative
import Data.Functor.Compose

-- | _Applicative_ transformer adding a list/nondeterminism/choice effect.
--   It is not a valid monad transformer, but it is a valid 'Applicative'.
newtype ListT m a = ListT {getListT :: Compose [] m a}
  deriving newtype (Functor, Applicative, Alternative)

lift :: m a -> ListT m a
lift = ListT . Compose . pure

runListT :: ListT m a -> [m a]
runListT = getCompose . getListT
