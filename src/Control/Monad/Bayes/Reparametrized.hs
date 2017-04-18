{-|
Module      : Control.Monad.Bayes.Reparametrized
Description : Decomposing distribution into a standard one and a deterministic transformation based on parameters
Copyright   : (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Reparametrized (
  Reparametrized,
  reparametrize
) where

import Data.Vector
import Data.Reflection (Reifies)
import Numeric.AD
import Numeric.AD.Mode.Reverse
import Numeric.AD.Internal.Reverse (Tape)
import Control.Monad.Trans (MonadTrans, MonadIO)
import Control.Monad.Trans.Identity (IdentityT(IdentityT))

import Statistics.Distribution.Polymorphic.Unconstrained
import qualified Statistics.Distribution.Polymorphic.Discrete as D
import qualified Statistics.Distribution.Polymorphic.Normal as N
import qualified Statistics.Distribution.Polymorphic.Uniform as U
import Control.Monad.Bayes.Simple

-- | A transformer that uses the reparametrization trick to automatically propagate gradients through sampling functions.
newtype Reparametrized s m a = Reparametrized (IdentityT m a)
  deriving(Read, Show, Eq, Functor, Applicative, Monad, MonadTrans, MonadIO)

instance (HasCustomReal m, Reifies s Tape) => HasCustomReal (Reparametrized s m) where
  type CustomReal (Reparametrized s m) = Reverse s (CustomReal m)

instance (Functor m, HasCustomReal m, r ~ CustomReal m, Reifies s Tape, Sampleable (Normal r) m)
        => Sampleable (Normal (Reverse s r)) (Reparametrized s m) where
  sample d = Reparametrized $ IdentityT $ fmap (\x -> (auto x + N.mean d) * N.stddev d) $ normal 0 1

instance (Functor m, HasCustomReal m, r ~ CustomReal m, Reifies s Tape, Sampleable (Uniform r) m)
        => Sampleable (Uniform (Reverse s r)) (Reparametrized s m) where
  sample d = Reparametrized $ IdentityT $ fmap (\x -> a + (b - a) * (auto x)) $ uniform 0 1 where
    a = U.lower d
    b = U.upper d

instance (Functor m, HasCustomReal m, r ~ CustomReal m, Reifies s Tape, Sampleable (Uniform r) m)
        => Sampleable (Discrete (Reverse s r) Int) (Reparametrized s m) where
  sample d = Reparametrized $ IdentityT $ fmap (select . auto) $ uniform 0 1 where
    select x = case findIndex (>= x) (scanl1' (+) (D.weights d)) of
                  Just i -> i
                  Nothing -> error "Reparametrized: bad weights in Discrete"

instance (Functor m, KnownSupport d, Sampleable d (Reparametrized s m)) => Sampleable (Unconstrained d) (Reparametrized s m) where
  sample d = fmap (transformConstraints d') (sample d') where
    d' = getConstrained d

-- | Reparametrize model to sample only from distributions with fixed parameters.
reparametrize :: Reparametrized s m a -> m a
reparametrize (Reparametrized (IdentityT m)) = m
