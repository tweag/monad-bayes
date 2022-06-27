{-# OPTIONS_GHC -Wno-orphans #-}
module Control.Monad.Bayes.Traced.Grad where
import Control.Monad.RWS
import Control.Monad.Bayes.Class
import Linear
import qualified Debug.Trace as Debug
import Numeric.AD
import Lens.Micro ((^.), over)
import Lens.Micro.Extras (view)
import Control.Monad.Bayes.Traced.Common (pdf)
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Free
import Numeric.AD.Mode.Reverse (Reverse)
import Numeric.Log (Log(Exp))


-- try = stormerVerlet2H
--   (1 :: Double)
--   (const (Ap [1,3]))
--   (const (Ap [1,3]))

-- | Störmer-Verlet integration scheme for systems of the form
-- \(\mathbb{H}(p,q) = T(p,q) + V(p,q)\)
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
stormerVerlet2H hh nablaQ nablaP prev = V2 qNew pNew
  where
    h2 = hh / 2
    hhs = pure hh
    hh2s = pure h2
    qsPrev = prev ^. _x
    psPrev = prev ^. _y
    pp2 = psPrev - hh2s * nablaQ qsPrev
    qNew = qsPrev + hhs * nablaP pp2
    pNew = pp2 - hh2s * nablaQ qNew

stormerVerlet2 ::
  (Applicative f, Num (f a), Fractional a) =>
  -- | \(f\)
  (f a -> f a) ->
  -- | Step size
  a ->
  -- | Current \((p, q)\) as a 2-dimensional vector
  V2 (f a) ->
  -- | New \((p, q)\) as a 2-dimensional vector
  V2 (f a)
stormerVerlet2 f h prev =
  let h' = h
      h2' = 0.5 * h
      p1 = prev ^. _x + pure h2' * f (prev ^. _y)
      q' = prev ^. _y + pure h' * p1
      p' = p1 + pure h2' * f q'
   in V2 p' q'

nrml :: Floating a => a -> a -> a -> a
nrml mu sigma x = 1 / (sigma * sqrt (2 * pi)) * exp ((-0.5) * (((x - mu) / sigma) ^^ 2))

getPhasePoint :: MonadSample n m => n -> m n (n, n)
getPhasePoint q = (,q) <$> randomGeneric -- TODO: fix to normal q 1

getPhasePoints :: MonadSample n m => [] n -> m n (V2 [n])
getPhasePoints x = uncurry V2 . unzip <$> Prelude.mapM getPhasePoint x

hamiltonian :: RealFloat n => 
  ([n] -> n) -> [n] -> [n] -> n
hamiltonian potential p q =
  negate (log $ potential q) - Prelude.sum (log (nrml q 1 p))


-- ex = grad (\x -> x ^. _x + x ^. _y)
-- traceIt x = Debug.trace (show x) x

instance (Num a) => Num ([a]) where
  ([a]) + (b) = fmap ((+) a) b
  (a) + ([b]) = fmap ((+) b) a
  (a) + (b) = zipWith (+) a b

  -- (a) * (b) =  zipWith (*) (traceIt "left" a) (traceIt "right" b)
  ([a]) * (b) = fmap ((*) a) b
  (a) * ([b]) = fmap ((*) b) a
  (a) * (b) = zipWith (*) a b
  abs (a) = abs <$> a
  negate (a) = negate <$> a
  signum (a) = signum <$> a
  fromInteger a = [fromInteger a]

instance (Fractional a) => Fractional ([a]) where
  fromRational a = [fromRational a]
  recip = fmap recip

instance Floating a => Floating ([a]) where
  pi = [pi]
  log = fmap log
  sin = fmap sin
  cos = fmap cos
  cosh = fmap cosh
  sinh = fmap sinh
  exp = fmap exp
  atan = fmap atan
  asin = fmap asin
  acosh = fmap acosh
  atanh = fmap atanh
  asinh = fmap asinh
  acos = fmap acos
  tan = fmap tan



squish x =
  -- trace (show x)
  (1 / (1 + exp (-x)))


hmcKernel :: (MonadSample n m, Show n) =>
-- (Weighted (FreeSampler IdentityN) (Reverse s a) a) ->
  (forall n. (RealFloat n, Show n) => [n] -> n) -> [n] -> m n [n]
hmcKernel potential  =
  fmap
    ( view _y
      . stepForward -- Prelude.foldr (.) id (Prelude.replicate 1 stepForward)
    )
  . getPhasePoints
  where
    h :: forall n. (Show n, RealFloat n) => [n] -> [n] -> n
    h = hamiltonian potential
    stepForward = stormerVerlet2H 0.1 ((grad $ h 0)) (grad (flip (h) 0))

model :: (RealFloat n, MonadInfer n m) => m n Bool
model = do
  -- return True
  x <- randomGeneric
  -- y <- randomGeneric
  scoreGeneric (Exp $ log $ x)
  return (x > 0.5)
  -- scoreGeneric (Exp $ log $ x ** 2 * y)
  -- return (x > 0.5)
  
-- example :: RealFloat n => [n] -> [n]
example :: (Show n, MonadSample n m) => [n]
  -> m n [n]
example = hmcKernel (pdf model)

-- instance (RealFloat n, Floating n, Ord n, Num n, Show n) => MonadSample' n (State [n]) where
--   -- random' ::
--   random' = do
--     stack <- get
--     -- return (head stack)
--     case stack of
--       [] -> return 0
--       (x : rest) -> (put rest) >> return (squish x) -- (undefined . cumulative (normalDistr 0 1)) x

-- instance (Monad m, MonadSample' n m, RealFloat n, Floating n, Ord n, Num n)
--   => MonadSample' n (Weighted' n (m)) where
-- random' = lift random'

-- prog2 :: (Floating n, RealFloat n, Ord n, Num n, Show n) => Weighted' n (State [n]) Bool
-- prog2 = do
--   x <-
--     rando ::
--       (RealFloat n, Floating n, Ord n, Num n, Show n) => ((Weighted' n (State [n])) n)
--   -- let x = undefined
--   (score' :: (Num n, RealFloat n) => Log n -> Weighted' n (State [n]) ())
--     (Exp $ log x)
--   -- (score'
--   -- :: (RealFloat n, Floating n, Ord n, Num n) => (n -> (Weighted' n (State [n])) ())) (4 :: (RealFloat n, Floating n, Ord n, Num n) => n)
--   -- (score' :: forall n  . MonadCond n => Log n -> m ()) undefined -- (4 :: forall n . (Floating n, RealFloat n, Num n, Ord n) => Log n)
--   return (x > 0)

-- fail :: IO (Bool, Log Double)
-- runFunction' :: (Show n, RealFloat n) =>
--   Weighted' n (State [n]) a -> [n] -> n
-- runFunction' p = ln . exp . snd . evalState (runWeighted' p)

-- checkNaN = hamiltonian . runFunction'

-- pr :: (RealFloat n, Num n) => [n] -> n
-- pr = runFunction' prog2

-- runFunction :: (Floating n, Ord n) => Weighted (State s) a -> s -> n
-- runFunction p = snd . evalState (runWeighted p)

-- example = sampleIO . hmcKernel (runFunction' prog2) -- [0.1, 0.1]

-- rando :: (Ord n, MonadState [n] m, Floating n, MonadTrans t, Monad (t m), Show n) => t m n
-- rando = do
--   stack <- lift get
--   -- return (head stack)
--   case stack of
--     [] -> undefined -- return 0
--     (x : rest) -> lift (put rest) >> return (squish x)

-- ex :: (MonadSample' Double m, MonadCond' Double m) => m Double
-- ex = do
--   x <- random'
--   y <- random'
--   score' (Exp $ log x)
--   return (x + y)

-- ex' :: (MonadSample' Double m) => m Double
-- ex' = do
--   x <- random'
--   y <- random'
--   -- score' (Exp $ log x)
--   return (x + y)

-- -- foo :: (MonadSample' Double m, MonadCond' Double m) => m
-- --   (Double, Log Double)
-- foo :: (Double, Log Double)
-- foo = evalState ((runWeighted') ex) [1 :: Double]

-- runWeighted'' :: Weighted' Double m a -> m (a, Log Double)
-- runWeighted'' = runWeighted'

-- bar = sampleIO $ runWeighted'' ex'

-- class Monad m => Sample n m where
--   rand :: m n

-- class Monad m => Con n m where
--   scor :: n -> m ()

-- type Dens n = State n

-- instance (Num n, Floating n) => Sample n (Dens n) where
--   rand = state (\x -> (x,1))

-- instance (Num n) => Con n (Dens n) where
--   scor d = state (\k -> ((), k*d))

-- te :: f Integer
--   -> f Integer
-- te = grad (Prelude.product . Prelude.take 4)

-- prog :: (Num n, Floating n, Con n m, Sample n m) => m n

-- ra ::
--   Num n =>
--   RWS
--     ()
--     (Product n)
--     [n]
--     n
-- ra = do
--   stack <- get
--   case stack of
--     [] -> return 0
--     (x : rest) -> put rest >> return x

-- -- sc :: (Num n, Monad m) => n -> Writ erT (Product n) m ()
-- sc :: RWS.MonadWriter (Product a) m => a -> m ()
-- sc d = RWS.tell $ Product d

-- prog :: (Floating n, Ord n) => RWS () (Product n) [n] Bool
-- prog = do
--   y <- ra
--   -- x <- ra
--   sc (nrml 0 1 y)
--   -- RWS.tell (if y >= 0.5 then 0 else 1)
--   -- RWS.tell (Product $ y**2 + x**2) -- (if y+x > 0.5 then 0 else 1)
--   --   x <- rand :: (Floating n, Num n) => State n n
--   --   co (y > 0.5)
--   --   -- scor (4 :: (Floating n, Num n) => n)
--   return (True)

-- run :: (Floating n, Ord n) => [n] -> n
-- run = getProduct . (\(_, _, x) -> x) . runRWS prog ()

-- co b = modify (* if b then 1 else 0)

-- ex4 :: Double -> Double
-- ex4 d = execState prog (d :: Double)

-- ex5 :: Double -> Double
-- ex5 d = evalState prog (d :: Double)

-- verlet :: (Applicative f, Fractional a, Traversable f, Num (f a)) =>
--   (V1 a -> V1 a -> a) -> V2 (f a) -> V2 (f a)

data HasDensity n a = HD (a -> n)

-- instance Num n => Contravariant (HasDensity n) where
diffmap ::
  (Floating n) =>
  (forall m. Floating m => m -> m) ->
  HasDensity n n ->
  HasDensity n n
-- diffmap :: (n -> a) -> HasDensity n a -> HasDensity n n
diffmap f (HD g) = HD (\x -> (g . f) x * abs (diff f x)) -- HD (f . undefned)

r :: HasDensity Double Double
r = HD f
  where
    f x
      | x > (-0.5) && x <= 0.5 = 1.0
      | otherwise = 0.0

-- ch :: HasDensity Unit Double
ch = diffmap (exp . negate . (** 2)) r