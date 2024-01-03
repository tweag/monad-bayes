{-# LANGUAGE StandaloneDeriving #-}

module Control.Applicative.List where

-- base
import Control.Applicative
import Data.Functor.Compose

-- * Applicative ListT

-- | _Applicative_ transformer adding a list/nondeterminism/choice effect.
--   It is not a valid monad transformer, but it is a valid 'Applicative'.
newtype ListT m a = ListT {getListT :: Compose m [] a}
  deriving newtype (Functor, Applicative, Alternative)

listT :: m [a] -> ListT m a
listT = ListT . Compose

lift :: (Functor m) => m a -> ListT m a
lift = ListT . Compose . fmap pure

runListT :: ListT m a -> m [a]
runListT = getCompose . getListT
