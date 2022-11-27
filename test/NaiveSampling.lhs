\documentclass{article}

%include polycode.fmt
%options ghci -isrc

%format currXi0 = "\xi_0"
%format xiMinus0 = "\mu_{\xi_{-0}}"
%format condDistXiMinus0GivenXi0 = "\mu_{\xi_0}"
%format phi = "\phi"
%format rho = "\rho"
%format testRho = "\tilde{\rho}"
%format xi = "\xi"
%format xi00 = "\zeta"
%format xi0 = "\xi_0"
%format xi' = "\xi^{\prime}"
%format alpha = "\alpha"
%format mu0 = "\mu_0"
%format mu = "\mu"
%format sigma = "\sigma"
%format sigma0 = "\sigma_0"
%format sigmaP = "\sigma_P"
%format truePosteriorMu = "\hat{\mu}"
%format truePosteriorSigma = "\hat{\sigma}"
%format qq = "Q"
%format varphi = "\varphi"
%format testVarphi = "\tilde{\varphi}"

\usepackage{amsmath}
\usepackage{mathrsfs}
\usepackage{amssymb}
\usepackage{natbib}

\usepackage[colorlinks, backref = page]{hyperref}
\hypersetup{
     colorlinks   = true,
     citecolor    = gray
}

\usepackage{svg}

\usepackage{graphicx}
\usepackage[left=0.25in, right=2.00in]{geometry}

\usepackage[textwidth=1.75in]{todonotes}

\setlength{\parskip}{\medskipamount}
\setlength{\parindent}{0pt}

\author{Dominic Steinitz}

\begin{document}

\title{A Haskell Perspective on a general perspective on the Metropolis–--Hastings kernel}

\maketitle

\listoftodos

\section{Introduction}

%if style == newcode
\begin{code}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wall              #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-orphans       #-}

module Main (
    main
  , gradUAD
  , algo1
  , truePosteriorMu
  , truePosteriorSigma
  , student5
  , singleObs
  , genEg
  ) where

import Control.Monad.Bayes.Class hiding (posteriorPredictive, Histogram)
import Control.Monad.Bayes.Sampler.Strict

import Numeric.AD

import qualified Data.ByteString.Lazy as BL
import Data.Csv.Incremental (encodeRecord, encode)
import Data.Csv hiding (encode)

import System.FilePath ()
import qualified System.Random.Stateful as S
import qualified System.Random as S
import qualified Data.Random as R
import qualified System.Random.MWC as MWC

import Statistics.Distribution.StudentT
import Statistics.Distribution.Normal
import Statistics.Distribution
import Statistics.Distribution.DiscreteUniform (discreteUniformAB)
import qualified Data.Vector as V
-- import Data.Matrix

import Data.Monoid
import Control.Monad.State
import Control.Monad.Trans.Writer
import Control.Monad.Reader

import "monad-extras" Control.Monad.Extra
\end{code}
%endif

Remarkably (to me at least) all\footnote{well almost all} MCMC
algorithms can be captured by one general algorithm. At the moment you
are expected to know how MCMC works to be able to read what follows. I
may add a section introducing MCMC later.

Here's {\bf Algorithm 1} from \cite{9117ec7157f141e28e5fe7bc575df48b}:

\begin{code}
algo1 :: Show a => Show b => (MonadDistribution m, Fractional t) =>
         (a, c) -> (a -> m b) -> ((a, b) -> (a, b)) -> (t -> Double) -> ((a, b) -> t) -> m (a, b)
algo1 (currXi0, _) condDistXiMinus0GivenXi0 phi a rho = do
  xiMinus0 <- condDistXiMinus0GivenXi0 currXi0
  let xi = (currXi0, xiMinus0)
  let alpha = a $ (rho . phi) xi / rho xi
  u <- random
  if u < alpha
    then return $ phi xi
    else return xi
\end{code}

Something very similar seems to have been discovered
in~\cite{cusumano2020automating} and
\cite{neklyudov2020involutive}. Serendipitously, all three papers call
this algorithm 1!.

\section{An Example with an Analytical Solution}

In Bayesian statistics we have a prior distribution for the unknown
mean which we also take to be normal

$$
\mu \sim \mathcal{N}\left(\mu_0, \sigma_0^2\right)
$$

and then use a sample

$$
x \mid \mu \sim \mathcal{N}\left(\mu, \sigma^2\right)
$$

to produce a posterior distribution for it

$$
\mu \mid x \sim \mathcal{N}\left(\frac{\sigma_{0}^{2}}{\sigma^{2}+\sigma_{0}^{2}} x+\frac{\sigma^{2}}{\sigma^{2}+\sigma_{0}^{2}} \mu_{0},\left(\frac{1}{\sigma_{0}^{2}}+\frac{1}{\sigma^{2}}\right)^{-1}\right)
$$

If we continue to take samples then the posterior distribution becomes

$$
\mu \mid x_{1}, x_{2}, \cdots, x_{n} \sim \mathcal{N}\left(\frac{\sigma_{0}^{2}}{\frac{\sigma^{2}}{n}+\sigma_{0}^{2}} \bar{x}+\frac{\sigma^{2}}{\frac{\sigma^{2}}{n}+\sigma_{0}^{2}} \mu_{0},\left(\frac{1}{\sigma_{0}^{2}}+\frac{n}{\sigma^{2}}\right)^{-1}\right)
$$

Note that if we take $\sigma_0$ to be very large (we have little prior
information about the value of $\mu$) then

$$
\mu \mid x_{1}, x_{2}, \cdots, x_{n} \sim \mathcal{N}\left(\bar{x},\left(\frac{1}{\sigma_{0}^{2}}+\frac{n}{\sigma^{2}}\right)^{-1}\right)
$$

and if we take $n$ to be very large then

$$
\mu \mid x_{1}, x_{2}, \cdots, x_{n} \sim \mathcal{N}\left(\bar{x},\frac{\sigma}{\sqrt{n}}\right)
$$

which ties up with the classical estimate.

Let's illustrate this with a few numbers.

\begin{code}
mu0, sigma0, sigma, sigmaP, z :: Floating a => a
mu0 = 0.0
sigma0 = 1.0
sigma = 1.0
sigmaP = 0.2
z = 4.0

truePosteriorMu :: Double
truePosteriorMu = z * sigma0^2 / (sigma^2 + sigma0^2) + mu0 * sigma^2 / (sigma^2 + sigma0^2)

truePosteriorSigma :: Double
truePosteriorSigma = sqrt $ recip (recip sigma0^2 + recip sigma^2)
\end{code}

This gives $\hat{\mu} = \eval{truePosteriorMu}$ and $\hat{\sigma} = \eval{truePosteriorSigma}$ which is what we would expect: we thought the mean was $\mu_0 = \eval{mu0}$ but we have an observation $z = \eval{z}$ and also the variance is now less.

\section{Using MCMC}

For us, we want the posterior

$$
\varpi(\mu) = \frac{1}{Z}\exp{\frac{(x - \mu)^2}{2\sigma^2}} \exp{\frac{(\mu - \mu_0)^2}{2\sigma_0^2}}
$$

where $x, \mu_0, \sigma$ and $\sigma_0$ are all given but $Z$ is unknown.

\subsection{Random Walk Metropolis}

Let's implement a traditional random walk. Here's the proposal distribution:

\begin{code}
qq :: Double -> Double -> Double
qq w w' = exp (-(w - w')^2 / (2 * sigmaP^2))
\end{code}

And here's the specification for $\rho$:

\begin{code}
testRho :: (a -> Double) -> (a -> b -> Double) -> (a, b) -> Double
testRho varphi q (w, w') = varphi w * q w w'
\end{code}

Here's the un-normalised posterior:

\begin{code}
testVarphi :: Floating a => a -> a
testVarphi mu = exp (-(z - mu)^2 / (2 * sigma^2)) * exp (-(mu - mu0)^2 / (2 * sigma0^2))
\end{code}

We can now use one step of the algorithm and then run it for as many times as we wish:

\begin{code}
testRwmOneStep :: MonadDistribution m => (Double, Double) -> m (Double, Double)
testRwmOneStep (xi0, _) = algo1 (xi0, undefined) condDistXiMinus0GivenXi0 phi a rho
  where
    phi = \(x, y) -> (y, x)
    a   = min 1.0
    rho = testRho testVarphi qq
    condDistXiMinus0GivenXi0 = \xi00 -> normal xi00 sigmaP

testRwm :: (Eq a, Num a, MonadDistribution m) =>
               a -> m [(Double, Double)]
testRwm n = unfoldM f (n, (1.0, 0.0 / 0.0))
  where
    f (0, _) = return Nothing
    f (m, s) = do x <- testRwmOneStep s
                  return $ Just (s, (m - 1, x))
\end{code}

And we can see the results in Figure~\ref{fig:Rwm}. A bit skewed but
we didn't burn in and the starting value is 1.0.

\begin{figure}[!htbp]
    \centering
    \includegraphics[width=0.8\textwidth]{diagrams/RwM.png}
    \caption{Random Walk Metropolis}
    \label{fig:Rwm}
\end{figure}

\subsection{Random walk Metropolis ratio}

Here's a different algorithm expressed using the generalised
approach. The results are in Figure~\ref{fig:MwMr}.

\begin{code}
testMwMrOneStep :: MonadDistribution m => (Double, Double) -> m (Double, Double)
testMwMrOneStep (xi0, _) = algo1 (xi0, undefined) condDistXiMinus0GivenXi0 phi a rho
  where
    phi = \(x, y) -> (x + y, -y)
    a   = min 1.0
    rho = testRho testVarphi (\_ -> \_ -> 1.0)
    condDistXiMinus0GivenXi0 = const (quantile (normalDistr 0.0 1.0) <$> random)

testMwMr :: (Eq a, Num a, MonadDistribution m) =>
               a -> m [(Double, Double)]
testMwMr n = unfoldM f (n, (1.0, 0.0 / 0.0))
  where
    f (0, _) = return Nothing
    f (m, s) = do x <- testMwMrOneStep s
                  return $ Just (s, (m - 1, x))
\end{code}

\begin{figure}[!htbp]
    \centering
    \includegraphics[width=0.8\textwidth]{diagrams/Mb.png}
    \caption{Random walk Metropolis ratio}
    \label{fig:MwMr}
\end{figure}

\subsection{What monad-bayes does}

Here's our toy problem expressed in monad-bayes:

\begin{code}
singleObs :: (MonadDistribution m, MonadFactor m) => m Double
singleObs = do
    mu <- normal mu0 sigma0
    factor $ normalPdf mu sigma z
    return mu
\end{code}

Here's what I think monad-bayes does with this using the General
Perspective. The results are in Figure~\ref{fig:MbPrime}.

\begin{code}
testMbOneStep :: MonadDistribution m => (Double, Double) -> m (Double, Double)
testMbOneStep (xi0, _) = algo1 (xi0, undefined) condDistXiMinus0GivenXi0 phi a rho
  where
    phi = \(x, y) -> (y, x)
    a   = min 1.0
    rho = testRho (\mu -> exp (-(z - mu)^2 / (2 * sigma^2))) (\_ -> \_ -> 1.0)
    condDistXiMinus0GivenXi0 = const (quantile (normalDistr mu0 sigma0) <$> random)

testMb :: (Eq a, Num a, MonadDistribution m) =>
               a -> m [(Double, Double)]
testMb n = unfoldM f (n, (1.0, 0.0 / 0.0))
  where
    f (0, _) = return Nothing
    f (m, s) = do x <- testMbOneStep s
                  return $ Just (s, (m - 1, x))
\end{code}

\begin{figure}[!htbp]
    \centering
    \includegraphics[width=0.8\textwidth]{diagrams/MbPrime.png}
    \caption{monad-bayes}
    \label{fig:MbPrime}
\end{figure}

\section{Some Mathematical Notes}

Suppose we don't know the classical MCMC algorithm. We can derive it from~\cite{9117ec7157f141e28e5fe7bc575df48b}:

$$
r\left(z, z^{\prime}\right)=\frac{\varpi\left(z^{\prime}\right) q\left(z^{\prime}, z\right)}{\varpi(z) q\left(z, z^{\prime}\right)}
$$

But where does this come from? We define $\mu$:

$$
\mu(\mathrm{d} \xi) \triangleq \pi\left(\mathrm{d} \xi_0\right) \mu_{\xi_0}\left(\mathrm{~d} \xi_{-0}\right)
$$

$$
\mu(\mathrm{d} (z, z^{\prime})) \triangleq \varpi\left(z\right)\mathrm{d} z ~ q_z\left(z^{\prime}\right)\mathrm{d} z^{\prime}
$$


Let $\mu$ be a finite measure on $(E, \mathscr{E}), \phi: E \rightarrow E$ an involution, let $\lambda \gg \mu$ be a $\sigma$-finite measure satisfying $\lambda \equiv \lambda^\phi$ and let $\rho=\mathrm{d} \mu / \mathrm{d} \lambda$. Then we can take $S=S\left(\mu, \mu^\phi\right)$ to be $S=\{\xi: \rho(\xi) \wedge \rho \circ \phi(\xi)>0\}$ and

$$
r(\xi)= \begin{cases}\frac{\rho \circ \phi}{\rho}(\xi) \frac{\mathrm{d} \lambda^\phi}{\mathrm{d} \lambda}(\xi) & \xi \in S, \\ 0 & \text { otherwise }\end{cases}
$$

So

$$
\rho(z, z^{\prime}) \triangleq \varpi\left(z\right) q_z\left(z^{\prime}\right)
$$

and with $\phi(z, z^{\prime}) = (z^{\prime}, z)$ we regain the familiar

$$
r\left(z, z^{\prime}\right)=\frac{\varpi\left({z^{\prime}}\right) q\left({z^{\prime}}, z\right)}{\varpi(z) q\left(z, z^{\prime}\right)}
$$

\section{A Probabilistic Programming Language}

\begin{code}
newtype Meas a = Meas (WriterT (Product Double, Sum Int) (State [Double]) a)
  deriving(Functor, Applicative, Monad)

bernoulli' :: Double -> Meas Bool
bernoulli' r = do
  x <- sample
  return $ x < r

sample :: Meas Double
sample = Meas $
       do ~(r:rs) <- get
          put rs
          tell $ (Product 1, Sum 1)
          return r

score' :: Double -> Meas ()
score' r = Meas $ tell $ (Product r, Sum 0)
\end{code}

\todo{The bus example}

\begin{code}
model1 :: Meas Bool
model1 = do
  x <- bernoulli' (2/7)
  let rate = if x then 3 else 10
  score' $ exp $ ln $ poissonPdf rate 4
  return x

draw :: ContDistr d => d -> Meas Double
draw d =  do x <- sample
             return $ quantile d x

normal' :: Double -> Double -> Meas Double
normal' m s = draw (normalDistr m s)

singleObs' = do
    mu <- normal' mu0 sigma0
    score' $ exp $ ln $ normalPdf mu sigma z
    return mu
\end{code}

The result is shown in Figure~\ref{fig:normalViaPpl}.

\begin{figure}[!htbp]
    \centering
    \includegraphics[width=0.8\textwidth]{diagrams/PplNormal.png.png}
    \caption{Normal via our PPL}
    \label{fig:normalViaPpl}
\end{figure}

\todo{We could also do with an example model with infinite paths}

\subsection{Importance Sampling}

\begin{code}
weightedsamples :: forall a . Meas a -> IO [(a,Double)]
weightedsamples (Meas m) = do

  let helper :: State [Double] [(a, (Product Double, Sum Int))]
      helper = unfoldM h undefined
        where
          h _ =  Just <$> (\v -> (v, undefined)) <$> runWriterT m

  g <- S.getStdGen
  let rs = S.randoms g
  let (xws,_) = runState helper rs
  return $ map (\(x, (w, _)) -> (x, getProduct w)) xws
\end{code}

\todo{Just to put my mind at rest, do some experiments with the generator being used twice (as below)}

\subsection{Metropolis Hastings}


\begin{code}
mhOneStep :: (MonadReader g m, R.StatefulGen g m) =>
             Meas a -> [Double] -> m [Double]
mhOneStep (Meas m) as = do
  let ((_, (w, l)), _) = runState (runWriterT m) as
  let n = getSum l
  as' <- do
    i <- R.sample $ R.uniform (0 :: Int) (n - 1)
    a' <- R.sample $ R.uniform 0.0 1.0
    case splitAt i as of
      (xs, _ : ys) -> return $ xs ++ (a' : ys)
      _ -> error "impossible"
  let ((_, (w', l')), _) = runState (runWriterT m) as'
  let n' = getSum l'
  let ratio = getProduct w' * (fromIntegral n') / (getProduct w * (fromIntegral n))
  a'' <- R.sample $ R.uniform 0.0 1.0
  if a'' < min 1 ratio then return as' else return as

iterateNM :: forall m a . Monad m => Int -> (a -> m a) -> a -> m [a]
iterateNM n f = foldr phi (return . return) (replicate n f)
  where
    phi :: (a -> m a) -> (a -> m [a]) -> a -> m [a]
    phi g h x = do y  <- g x
                   ys <- h y
                   return (y : ys)

testMh :: IO ()
testMh = do
  S.setStdGen (S.mkStdGen 43)
  g <- S.newStdGen
  stdGen <- S.newIOGenM g
  let rs = S.randoms g
  print $ take 10 rs
  is <- runReaderT (mhOneStep model1 rs) stdGen
  js <- runReaderT (iterateNM 1000000 (mhOneStep model1) rs) stdGen
  let (Meas n) = model1
  let foo :: [(Bool, Product Double) ]= map (\(x, (w, _)) -> (x, w)) $
            map (\as -> fst $ runState (runWriterT n) as) $
            js
  let l = length $ filter fst foo
  print l

testMh' :: IO [Double]
testMh' = do
  S.setStdGen (S.mkStdGen 43)
  g <- S.newStdGen
  stdGen <- S.newIOGenM g
  let rs = S.randoms g
  print $ take 10 rs
  js <- runReaderT (iterateNM 1100000 (mhOneStep singleObs') rs) stdGen
  let (Meas n) = singleObs'
  let foo :: [(Double, Product Double) ]= map (\(x, (w, _)) -> (x, w)) $
            map (\as -> fst $ runState (runWriterT n) as) $
            js
  return $ drop 100000 $ map fst foo
\end{code}

\section{Non-Parametric Hamiltonian Monte Carlo}

%% \begin{code}

%% npHmcStep q_0 w eta bigL = do
%%   let l = length q_0
%%   p0 <- sequence (replicate l (normal 0.0 1.0))
%%   let bigU = \n q -> -log(sum
%%   return undefined
%% def NPHMCstep(q0,w,ep,L):
%% # initialisation
%% p0 = [normal for i in range(len(q0))]
%% U = lambda n: lambda q:
%% −log(sum([w(q[:i]) for i in range(n)]))
%% # NP−HMC integration
%% ((q,p),(q0,p0)) = NPint((q0,p0),U,ep,L)
%% # MH acceptance
%% if cdfN(normal) < accept((q,p),(q0,p0),w):
%% return supported(q,w)
%% else:
%% return supported(q0,w)
%% def NPHMC(q0,w,ep,L,M):
%% S = [q0]
%% for i in range(M):
%% S.append(NPHMCstep(S[i],w,ep,L))
%% return S


%% def extend((q,p),(q0,p0),t,U):
%% while q not in domain(U(len(q))):
%% x0 = normal
%% y0 = normal
%% x = x0 + t*y0
%% y = y0
%% q0.append(x0)
%% p0.append(y0)
%% q.append(x)
%% p.append(y)
%% return ((q,p),(q0,p0))
%% def NPint((q0,p0),U,ep,L):
%% q = q0
%% p = p0
%% for i in range(L):
%% p = p − ep/2*grad(U(len(q0)),q)
%% q = q + ep*p
%% ((q,p),(q0,p0)) =
%% extend((q,p),(q0,p0),i*ep,U)
%% p = p − ep/2*grad(U(len(q0)),q)
%% return ((q,p),(q0,p0))

%% \end{code}

\section{Student's T}

Let's try running it on Student's T with 5 degrees of freedeom using
what I hope the textbook presentation of Metropolis---Hastings. The
probability density function (aka the Radon-Nikodym derivative wrt
Lebesgue measure) is

$$
f(t)=\frac{8}{3 \pi \sqrt{5}\left(1+\frac{t^2}{5}\right)^3}
$$

It's traditional to have $q_z(\cdot) \sim {\mathcal{N}}(z, \sigma_p^2)$ for some given $\sigma_p$.

Here's the density function for Student's T with 5 degrees of
freedom. We've defined it in terms of an un-normalised density so that
we can pretend we don't know the normalisation constant but still
sample from the distribution via MCMC.

\begin{code}
student5U :: Floating a => a -> a
student5U t =  1 / (1 + t^2/5)^3

student5 :: Floating a => a -> a
student5 t = student5U t * 8 / (3 * pi * sqrt 5)
\end{code}

Again, we can now use one step of the algorithm and then run it for as
many times as we wish. We instantiate the algorithm to be a Random
Walk Metropolis.

\begin{code}
testStudentRwmOneStep :: MonadDistribution m =>
                         (Double, Double) -> m (Double, Double)
testStudentRwmOneStep (xi0, _) = algo1 (xi0, undefined) (\xi00 -> normal xi00 sigmaP)
                                       (\(x, y) -> (y, x)) (min 1.0) (testRho student5U qq)

testStudentRwm :: (Eq a, Num a, MonadDistribution m) =>
                 a -> m [(Double, Double)]
testStudentRwm n = unfoldM f (n, (0.0, 0.0 / 0.0))
  where
    f (0, _) = return Nothing
    f (m, s) = do x <- testStudentRwmOneStep s
                  return $ Just (s, (m - 1, x))
\end{code}

We can also instantiate it with what I think monad-bayes does.

\begin{code}
testStudentMbOneStep :: MonadDistribution m => (Double, b) -> m (Double, Double)
testStudentMbOneStep (xi0, _) = algo1 (xi0, undefined) (const ((quantile (studentT 5)) <$> random))
                                      (\(x, y) -> (x + y, -y)) (min 1.0) (student5U . fst)

testStudentMb :: (Eq a, Num a, MonadDistribution m) =>
                 a -> m [(Double, Double)]
testStudentMb n = unfoldM f (n, (0.0, 0.0 / 0.0))
  where
    f (0, _) = return Nothing
    f (m, s) = do x <- testStudentMbOneStep s
                  return $ Just (s, (m - 1, x))
\end{code}

The results are shown in Figure~\ref{fig:StudRwm} and Figure~\ref{fig:StudMb}.

\begin{figure}[!htbp]
    \centering
    \includegraphics[width=0.8\textwidth]{diagrams/StudRwm.png}
    \caption{Random Walk Metropolis Student's T 5}
    \label{fig:StudRwm}
\end{figure}

\begin{figure}[!htbp]
    \centering
    \includegraphics[width=0.8\textwidth]{diagrams/StudMb.png}
    \caption{Monad Bayes Student's T 5}
    \label{fig:StudMb}
\end{figure}

\section{Hamiltonian Monte Carlo}

We'd like to put HMC into the same general framework but at the
moment, I am having trouble squaring Example 14 in
\cite{9117ec7157f141e28e5fe7bc575df48b} with the algorithm given in
\cite{Neal2011MCMCUH} (and I haven't even looked in
\cite{https://doi.org/10.48550/arxiv.1410.5110}). Here's as far as I
got with Student's t-distribution of degree 5. There's something going
on with exponentiating the Hamiltonian which I don't understand yet
either.

\todo{I think I have squared some of this but need to write this down}

Here's Student's T again:

$$
f(t) = \frac{8}{3 \pi \sqrt{5}\left(1+\frac{t^2}{5}\right)^3}
$$

Unnormalised:

$$
g(t) = \frac{1}{\left(1+\frac{t^2}{5}\right)^3}
$$

And as the potential energy part of the Hamiltonian:

$$
U(t) = -\log{g(t)} = 3 \log{(1 + \frac{r^2}{5})}
$$

Here's a version of the leapfrog algorithm:

\todo{This needs a bit more explanation}

\begin{code}
leapfrog :: Fractional a => a -> Int -> (a -> a) -> (a, a) -> (a, a)
leapfrog epsilon l gradU (qPrev, p) = (q1, p3)
  where
  p' =  p - epsilon * gradU qPrev / 2
  f 0 (qOld, pOld) = r
    where
      qNew = qOld + epsilon * pOld
      pNew = pOld
      r = (qNew, pNew)
  f _ (qOld, pOld) = r
    where
      qNew = qOld + epsilon * pOld
      pNew = pOld - epsilon * gradU qNew
      r = (qNew, pNew)
  (q1, p1) = foldr f (qPrev, p') ([0 .. l - 1])
  p2 = p1 - epsilon * gradU q1 / 2
  -- Is this necessary?
  p3 = negate p2
\end{code}

This is the Hamiltonian:
\todo{This needs a bit more explanation}

\begin{code}
rhoHmc :: Floating a => (b -> a) -> (b, a) -> a
rhoHmc u (q, p) = pU * pK
  where
    pU = recip $ u q
    pK = exp $ p^2 / 2
\end{code}

We need the derivative of the potential energy for the leapfrog
method. We could use automatic differentiation of course.

\begin{code}
gradU :: Fractional a => a -> a
gradU r =  3 * (2 * r / 5) / (1 + (r^2) / 5)

bigU :: Floating a => a -> a
bigU = negate . log . student5U

gradUAD :: Floating a => a -> a
gradUAD w = case grad (\[x] -> bigU x) $ [w] of
              [y] -> y
              _   -> error "Whatever"
\end{code}

And now we can run the sampler. The results are in~\ref{fig:Hmc}.

\begin{figure}[!htbp]
    \centering
    \includegraphics[width=0.8\textwidth]{diagrams/Hmc.png}
    \caption{Hamiltonian Monte Carlo Student's T 5}
    \label{fig:Hmc}
\end{figure}

\begin{code}
eta :: Fractional a => a
eta = 0.3

bigL :: Int
bigL = 10

testHmcOneStep :: MonadDistribution m => (Double, Double) -> m (Double, Double)
testHmcOneStep (xi0, _) = algo1 (xi0, undefined) condDistXiMinus0GivenXi0 phi a rho
  where
    phi = leapfrog eta bigL gradU
    a   = min 1.0
    rho = rhoHmc student5U
    condDistXiMinus0GivenXi0 = const $ normal 0.0 1.0

testHmc :: (Eq a, MonadDistribution m, Num a) =>
                a -> m [(Double, Double)]
testHmc n = unfoldM f (n, (0.0, 0.0))
  where
    f (0, _) = return Nothing
    f (m, s) = do a <- testHmcOneStep s
                  return $ Just (s, (m - 1, a))
\end{code}

%if style == newcode
\begin{code}
writeToFile :: (Int -> SamplerIO [(Double, Double)]) -> FilePath -> Int -> IO ()
writeToFile t p n = do
  rs <- sampler (t n)
  let ss = zip ((map show [1 :: Int ..]) :: [String]) (map fst rs)
  BL.writeFile (p <> (show $ n) <> ".csv") $ encode $
    foldMap encodeRecord ss

instance ToField Bool where
  toField False = "False"
  toField True  = "True"

main :: IO ()
main = do
  -- writeToFile testHmc        "StudSampsHMC"    10000
  -- writeToFile testStudentRwm "StudSampsRwm"  100002
  -- writeToFile testStudentMb  "StudSampsMb"    10003
  -- writeToFile testRwm        "Rwm"           100000
  -- writeToFile testMwMr       "MwMr"          100000
  -- writeToFile testMb         "MbPrime"      1000000
  -- (encode <$> foldMap encodeRecord <$> take 10000 <$> weightedsamples model1) >>=
  --   BL.writeFile "I.csv"
  (encode <$> foldMap encodeRecord <$> map Only <$> take 1000000 <$> testMh') >>=
    BL.writeFile "J.csv"
\end{code}
%endif

\todo{It should make no difference what we return in the momentum
position; yet it does? Maybe not but at least investigate it.}

\section{Gen}

Gen is a probabilistic programming language. I've taken the example from~\cite{cusumano2020automating} and converted it to use monad-bayes.

\todo{It's not clear this is a useful section but who knows?}

\begin{code}
genEg :: MonadDistribution m => Int -> m [Double]
genEg n = do
  k <- (+ 1) <$> poisson 1.0
  means <- replicate k <$> normal 0.0 10.0
  gammas <- replicate k <$> gamma 1.0 10.0
  let invGammas = map recip gammas
  weights <- dirichlet (V.replicate k 2.0)
  replicate n <$> (categorical weights >>= \i -> normal (means!!i) (invGammas!!i))

\end{code}

\subsection{Sam's Original Code (with minor mods)}

\begin{code}
mh :: Meas a -> [Double] -> State [Double] [Double]
mh (Meas m) bs = do
  let step :: [Double] -> State [Double] [Double]
      step as = do
        let ((_, (w,l)),_) = runState (runWriterT m) as
        r <- getrandom
        let i = categ (replicate (fromIntegral $ getSum l) (1/(fromIntegral $ getSum l))) r
        r' <- getrandom
        let as' = (let (as1,_:as2) = splitAt (fromIntegral i) as in as1 ++ r' : as2)
        let ((_, (w',l')),_) = runState (runWriterT m) (as')
        let ratio = getProduct w' * (fromIntegral $ getSum l') / (getProduct w * (fromIntegral $ getSum l))
        r'' <- getrandom
        if r'' < (min 1 ratio) then return as' else return as
  step bs

mhTest :: forall a . Meas a -> [(a, Product Double)]
mhTest m@(Meas n) = foo
  where
  (ss :: [[Double]], _) = runState (iterateM (mh m) (S.randoms (S.mkStdGen 42))) (S.randoms (S.mkStdGen 1729))
  foo = map (\(x, (w, _)) -> (x, w)) $
        map (\as -> fst $ runState (runWriterT n) as) $
        ss


getrandom :: State [Double] Double
getrandom = do
  ~(r:rs) <- get
  put rs
  return r

categ :: [Double] -> Double -> Integer
categ rs r = let helper (r':rs') r'' i =
                          if r'' < r' then i else helper rs' (r''-r') (i+1)
                 helper _ _ _ = error "categ"
           in helper rs r 0

\end{code}

\section{Bibliography}

\bibliographystyle{plain}
\bibliography{references.bib}

\end{document}
