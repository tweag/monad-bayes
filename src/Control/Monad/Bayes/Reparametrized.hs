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
  reparametrize,
  sg
) where

import Data.Vector
import Numeric.AD
import Numeric.AD.Mode.Reverse
import Control.Monad.Trans (MonadTrans, MonadIO, lift)
import Control.Monad.Trans.Identity (IdentityT(IdentityT))
import Control.Monad.Reader (ask)
import Control.Monad.Trans.State (runState)
import System.Random.MWC (save)

import qualified Statistics.Distribution.Polymorphic.Discrete as D
import qualified Statistics.Distribution.Polymorphic.Normal as N
import qualified Statistics.Distribution.Polymorphic.Uniform as U
import Control.Monad.Bayes.Simple
import Control.Monad.Bayes.Sampler

newtype Reparametrized t m a = Reparametrized (IdentityT m a)
  deriving(Read, Show, Eq, Functor, Applicative, Monad, MonadTrans, MonadIO)

instance (HasCustomReal m, CustomReal m ~ Scalar t, Floating t, Ord t) => HasCustomReal (Reparametrized t m) where
  type CustomReal (Reparametrized t m) = t

instance (Functor m, Floating t, Mode t, HasCustomReal m, Scalar t ~ CustomReal m,
          Sampleable (Normal (Scalar t)) m) => Sampleable (Normal t) (Reparametrized t m) where
  sample d = Reparametrized $ IdentityT $ fmap (\x -> (auto x - N.mean d) / N.stddev d) $ normal 0 1

instance (Functor m, Floating t, Mode t, HasCustomReal m, Scalar t ~ CustomReal m,
          Sampleable (Uniform (Scalar t)) m) => Sampleable (Uniform t) (Reparametrized t m) where
  sample d = Reparametrized $ IdentityT $ fmap (\x -> a + (b - a) * (auto x)) $ uniform 0 1 where
    a = U.lower d
    b = U.upper d

instance (Functor m, Floating t, Ord t, Mode t, HasCustomReal m, Scalar t ~ CustomReal m,
          Sampleable (Uniform (Scalar t)) m) => Sampleable (Discrete t Int) (Reparametrized t m) where
  sample d = Reparametrized $ IdentityT $ fmap (select . auto) $ uniform 0 1 where
    select x = case findIndex (>= x) (scanl1' (+) (D.weights d)) of
                  Just i -> i
                  Nothing -> error "Reparametrized: bad weights in Discrete"

-- | Reparametrize model to sample only from distributions with fixed parameters.
reparametrize :: Reparametrized t m a -> m a
reparametrize (Reparametrized (IdentityT m)) = m

-- | Stochastic gradient using a single sample
sg :: (forall s. [Reverse s Double] -> Reparametrized (Reverse s Double) SamplerST (Reverse s Double))
   -> [Double] -> SamplerST [Double]
sg model = gradM (reparametrize . model)
