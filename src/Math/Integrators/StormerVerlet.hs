module Math.Integrators.StormerVerlet
  ( integrateV,
    stormerVerlet2H,
    Integrator,
  )
where

import Control.Lens
import Control.Monad.Primitive
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Data.Vector.Mutable
import Linear (V2 (..))

-- | Integrator function
-- -   \Phi [h] |->  y_0 -> y_1
type Integrator a =
  -- | Step
  Double ->
  -- | Initial value
  a ->
  -- | Next value
  a

-- | Störmer-Verlet integration scheme for systems of the form
-- \(\mathbb{H}(p,q) = T(p) + V(q)\)
stormerVerlet2H ::
  (Applicative f, Num (f a), Fractional a) =>
  -- | Step size
  a ->
  -- | \(\frac{\partial H}{\partial q}\)
  (f a -> f a) ->
  -- | \(\frac{\partial H}{\partial p}\)
  (f a -> f a) ->
  -- | Current \((p, q)\) as a 2-dimensional vector
  V2 (f a) ->
  -- | New \((p, q)\) as a 2-dimensional vector
  V2 (f a)
stormerVerlet2H hh nablaQ nablaP prev =
  V2 qNew pNew
  where
    h2 = hh / 2
    hhs = pure hh
    hh2s = pure h2
    qsPrev = prev ^. _1
    psPrev = prev ^. _2
    pp2 = psPrev - hh2s * nablaQ qsPrev
    qNew = qsPrev + hhs * nablaP pp2
    pNew = pp2 - hh2s * nablaQ qNew

-- |
-- Integrate ODE equation using fixed steps set by a vector, and returns a vector
-- of solutions corrensdonded to times that was requested.
-- It takes Vector of time points as a parameter and returns a vector of results
integrateV ::
  PrimMonad m =>
  -- | Internal integrator
  Integrator a ->
  -- | initial  value
  a ->
  -- | vector of time points
  Vector Double ->
  -- | vector of solution
  m (Vector a)
integrateV integrator initial times = do
  out <- new (V.length times)
  write out 0 initial
  compute initial 1 out
  V.unsafeFreeze out
  where
    compute y i out
      | i == V.length times = return ()
      | otherwise = do
        let h = (times ! i) - (times ! (i - 1))
            y' = integrator h y
        write out i y'
        compute y' (i + 1) out
