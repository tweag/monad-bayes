{-|
Module      : Control.Monad.Bayes.Constraint
Description : Transformer mapping random variables onto unconstrained real line
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

-- It is safe to enable UndecidableInstances here so long as we don't write any Sampleable instances
-- where m doesn't shrink.
{-# LANGUAGE
  UndecidableInstances
 #-}

module Control.Monad.Bayes.Constraint (
  Constraint,
  hoist,
  unconstrain
) where

import Control.Monad.Trans
import Control.Monad.Trans.Identity

import Statistics.Distribution.Polymorphic.Unconstrained (inverseTransformConstraints)
import Control.Monad.Bayes.Simple

-- | Transformer mapping continuous random variables onto the unconstrained real line.
newtype Constraint m a = Constraint (IdentityT m a)
  deriving(Functor, Applicative, Monad, MonadTrans, MonadIO)

instance HasCustomReal m => HasCustomReal (Constraint m) where
  type CustomReal (Constraint m) = CustomReal m

instance {-# OVERLAPPING #-} (Functor m, KnownSupport d, RealNum d ~ Domain d, Sampleable (Unconstrained d) m) => Sampleable d (Constraint m) where
  sample d = Constraint $ IdentityT $ fmap (inverseTransformConstraints d) $ sample $ removeConstraints d

instance {-# OVERLAPPING #-} Sampleable (Discrete r k) m => Sampleable (Discrete r k) (Constraint m) where
  sample = Constraint . IdentityT . sample

instance {-# OVERLAPPING #-} Sampleable MVNormal m => Sampleable MVNormal (Constraint m) where
  sample = Constraint . IdentityT . sample

instance {-# OVERLAPPING #-} (Sampleable (Unconstrained d) m) => Sampleable (Unconstrained d) (Constraint m) where
  sample = Constraint . IdentityT . sample

instance Conditionable m => Conditionable (Constraint m) where
  factor = Constraint . IdentityT . factor

instance (MonadDist m, Sampleable (Unconstrained (Normal (CustomReal m))) m,
          Sampleable (Unconstrained (Gamma (CustomReal m))) m, Sampleable (Unconstrained (Beta (CustomReal m))) m,
          Sampleable (Unconstrained (Uniform (CustomReal m))) m) => MonadDist (Constraint m)
instance (MonadBayes m, Sampleable (Unconstrained (Normal (CustomReal m))) m,
          Sampleable (Unconstrained (Gamma (CustomReal m))) m, Sampleable (Unconstrained (Beta (CustomReal m))) m,
          Sampleable (Unconstrained (Uniform (CustomReal m))) m) => MonadBayes (Constraint m)

-- | Apply a transformation to the unconstrained monad.
hoist :: (m a -> n a) -> Constraint m a -> Constraint n a
hoist f (Constraint m) = Constraint (mapIdentityT f m)

-- | Convert sampling from univariate continuous distributions to sampling from corresponding 'Unconstrained'
-- distributions.
unconstrain :: Constraint m a -> m a
unconstrain (Constraint m) = runIdentityT m
